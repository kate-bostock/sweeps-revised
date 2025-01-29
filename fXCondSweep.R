# title: Plotting sweep probabilities
# author: Alexander Stein

#################
### libraries ###
#################
library("tidyverse")
library("RColorBrewer")

#setwd("/Users/stein02/Desktop/upload/Figures/supp_6/")

###################
### Import data ###
###################

### Simulation data
params <- read_csv("simulation_data_revised/params.csv")
probs <- read_csv("simulation_data_revised/sweeps-prob-descendants.csv")

param_wt <- read_csv("simulation_data_original/params_wt.csv")
meas_wt <- read_csv("simulation_data_original/wt_speeds.csv")

param_mut <- read_csv("simulation_data_original/params_mut.csv")
meas_mut <- read_csv("simulation_data_original/mut_speeds.csv")

# Merge the parameters with the data dataframe
df <- merge(probs, params, by.x = "id" , by.y = "index" )
df_wt <- merge(meas_wt, param_wt, by.x = "id" , by.y = "index" )
df_mut <- merge(meas_mut, param_mut, by.x = "id" , by.y = "index" )

### Numerical data
df_num <- read_csv("numerical_data/X conditioned on sweep.csv")

df_num <- rename(df_num, c("xval_twodim"="xval 2D", "yval_twodim"="yval 2D"))
df_num <- rename(df_num, c("xval_threedim"="xval 3D", "yval_threedim"="yval 3D"))

df_num$yval_twodim <- as.complex(df_num$yval_twodim)
df_num$yval_twodim <- Re(df_num$yval_twodim)

df_num$yval_threedim <- as.complex(df_num$yval_threedim)
df_num$yval_threedim <- Re(df_num$yval_threedim)


###################
### Computation ###
###################

# Filter data of interest
df_wt_f <- df_wt %>% filter(log2_deme==4, migration_rate==0.05, 
                            normal_birth_rate>=0.9, normal_birth_rate<=0.95)
# Compute wildtype speed from data
c_wt <- mean(df_wt_f$speed_late)

# Compute the mutant speed from the data
df_mut_f <- df_mut %>% filter(log2_deme==4, migration_rate==0.05, s_driver_birth==0.3)

c_mut <- mean(df_mut_f$speed_late)

# Define paramters of the simulation
pi <- 3.1415
mig <- 0.05
s_wt <- 0.1
demesize <- 16
# Compute Fisher speed of the wildtype
c_Fisher <- 2*sqrt(s_wt*(mig*demesize)/2)

s_vector = c(0.1,0.2,0.3,0.4,0.5,1.0,1.5,2.0)
mu_vector = c(1e-06,1e-05,1e-04,1e-03,1e-02)

# Filter data of interest
mu <- 1e-05
s_m <- 0.3
rho <- (1-1/(1+s_m))/(1-1/(1+s_m)^demesize)
df_f <- df %>% filter(mu_driver_birth == mu)
df_ff <- df_f %>% filter(s_driver_birth == s_m)
df_ff <- df_ff %>% filter(x100 == TRUE)

# Compute analytic results
#speeds <- df_ff$pop_radius_at_first_mut_init/df_ff$first_mutant_init_time
#c_wt_data <- mean(speeds)

theta_2D <- (3*c_wt/(pi*rho*mu))^(1/3)
gamma <- (c_mut-c_wt)/c_mut
c_m <- 2*sqrt(s_m*(mig*demesize)/2)

xaxis <- 1:250
f_X <- 3*xaxis^2*exp(-xaxis^3/(theta_2D^3*gamma^2))/(theta_2D^3*gamma^2)


################
### Plotting ###
################

cols <- brewer.pal(12,"Paired")

legend_1 <- "simulation"
legend_2 <- "prediction (exact)"
legend_3 <- "prediction (approx.)"

colours <- t(c(cols[4],cols[3]))
colnames(colours) <- c(legend_2, legend_3)

lty <- t(c("22", "solid"))
colnames(lty) <- c(legend_2, legend_3)

ggplot() + geom_histogram(aes(x=df_ff$pop_radius_at_first_mut_init, y=..density..), fill=cols[4], alpha=0.25, bins=21) +
  geom_line(aes(x=df_num$xval_twodim, y=df_num$yval_twodim,color=legend_2, linetype=legend_2), linewidth=1.5) +
  geom_line(aes(x=xaxis, y=f_X,color=legend_3,linetype=legend_3), linewidth=1.5) +
  labs(x="population radius, *x*", y="*f<sub>X*(*X=x*|sweep)") +
  #geom_label(aes(x=c(15),y=c(0.2), label=paste("mu = ", mu, " and s = ", s_m,  sep = "") ), size=6 ) +
  xlim(0,100) + theme_bw(base_size = 25) +
  scale_color_manual(values = colours) +
  scale_linetype_manual(values = lty) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.7,0.8),legend.title = element_blank())


# ### Save the figure
# 
# fig <- ggplot() + geom_histogram(aes(x=df_ff$pop_radius_at_first_mut_init, y=..density..), fill=cols[4], alpha=0.25, bins=21) +
#   geom_line(aes(x=df_num$xval_twodim, y=df_num$yval_twodim,color=legend_2, linetype=legend_2), linewidth=1.5) +
#   geom_line(aes(x=xaxis, y=f_X,color=legend_3,linetype=legend_3), linewidth=1.5) +
#   labs(x="population radius, *x*", y="*f<sub>X*(*X=x*|sweep)") +
#   #geom_label(aes(x=c(15),y=c(0.2), label=paste("mu = ", mu, " and s = ", s_m,  sep = "") ), size=6 ) +
#   xlim(0,70) + theme_bw(base_size = 25) +
#   scale_color_manual(values = colours) +
#   scale_linetype_manual(values = lty) +
#   #theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(),legend.position = "none",legend.title = element_blank())
#   theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), legend.position = c(0.7,0.8),legend.title = element_blank())
# 
# #a = 1.5
#text <- paste("/Users/stein02/Desktop/upload/Figures/supp_6/figures/","mu", mu, "s", s_m, ".png",sep = "")
#ggsave(text, fig, width = 4^a, height = 3^a)










