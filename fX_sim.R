# title: Plotting sweep probabilities
# author: Alexander Stein

#################
### libraries ###
#################
library("tidyverse")
library("ggtext")
library("RColorBrewer")

#1setwd("/Users/stein02/Desktop/upload/Figures/Figure_3/")

###################
### Import data ###
###################

params <- read_csv("simulation_data_original/params.csv")
probs <- read_csv("simulation_data_original/sweeps-prob.csv")

param_wt <- read_csv("simulation_data_original/params_wt.csv")
meas_wt <- read_csv("simulation_data_original/wt_speeds.csv")

# Merge the parameters with the data dataframe
df <- merge(probs, params, by.x = "id" , by.y = "index" )
df_wt <- merge(meas_wt, param_wt, by.x = "id" , by.y = "index" )

###################
### Computation ###
###################

# Filter data of interest
df_wt_f <- df_wt %>% filter(log2_deme==4, migration_rate==0.05, 
                            normal_birth_rate>=0.9, normal_birth_rate<=0.95)
# Compute wildtype speed from data
c_wt <- mean(df_wt_f$speed_late)

# Compute Fisher speed
mig <- 0.05
s_wt <- 0.1
demesize <- 16
c_Fisher <- 2*sqrt(s_wt*(mig*demesize)/2)

#s_vector = c(0.1,0.2,0.3,0.4,0.5,1.0,1.5,2.0)
#mu_vector = c(1e-06,1e-05,1e-04,1e-03,1e-02)

# Filter data of interest
mu <- 1e-05
s_m <- 0.3
rho <- (1-1/(1+s_m))/(1-1/(1+s_m)^demesize)
pi <- 3.1415
df_f <- df %>% filter(mu_driver_birth == mu)
df_ff <- df_f %>% filter(s_driver_birth == s_m)

# Compute analytic results
#speeds <- df_ff$pop_radius_at_first_mut_init/df_ff$first_mutant_init_time
#c_wt <- mean(speeds)

theta_2D <- (3*c_wt/(pi*rho*mu))^(1/3)

xaxis <- 1:250
f_X <- 3*xaxis^2*exp(-xaxis^3/theta_2D^3)/theta_2D^3

#####################
### Make the plot ###
#####################

cols <- brewer.pal(12,"Paired")

legend_1 <- "simulation"
legend_2 <- "prediction"

colours <- t(c(cols[4], "transparent"))
colnames(colours) <- c(legend_1, legend_2)

lty <- t(c("solid", "blank"))
colnames(lty) <- c(legend_1, legend_2)

# ggplot() + geom_histogram(aes(x=df_ff$pop_radius_at_first_mut_init, y=..density..,linetype=legend_1),
#                           fill=cols[4], alpha=0.25, bins=15) +
#   geom_line(aes(x=xaxis, y=f_X, linetype=legend_2), col=cols[4], linewidth = 1.5) +
#   labs(x="population radius, *x*", y="*f<sub>X*(*x*)") +
#   #geom_label(aes(x=c(8),y=c(0.250), label=paste("mu = ", mu, " and s = ", s_m,  sep = "") ), size=6 ) +
#   xlim(0,100) + 
#   #scale_color_manual(values = colours) +
#   scale_linetype_manual(values = lty) +
#   theme_bw(base_size = 25) +
#   theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
#         legend.position = c(0.8,0.8), legend.title = element_blank())


### Saving the figure

fig <- ggplot() + geom_histogram(aes(x=df_ff$pop_radius_at_first_mut_init, y=..density..,linetype=legend_1),
                                   fill=cols[4], alpha=0.25, bins=30) + #KateB -> changed to 30 from 15 to get exact chart in paper 
  geom_line(aes(x=xaxis, y=f_X, linetype=legend_2), col=cols[4], linewidth = 1.5) +
  labs(x="population radius, *x*", y="*f<sub>X*(*x*)") +
  #geom_label(aes(x=c(8),y=c(0.250), label=paste("mu = ", mu, " and s = ", s_m,  sep = "") ), size=6 ) +
  xlim(0,100) + 
  #scale_color_manual(values = colours) +
  scale_linetype_manual(values = lty) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.8,0.8),legend.title = element_blank())

fig 

a = 1.5
#ggsave("/Users/stein02/Desktop/upload/Figures/Figure_3/figures/fX_sim.png", fig, width = 4^a, height = 3^a)



            