# title: Plotting sweep probabilities
# author: Alexander Stein

#################
### libraries ###
#################

library("tidyverse")
library("ggtext")
library("expint")
library("RColorBrewer")

###################
### Import data ###
###################

#setwd("/Users/stein02/Desktop/upload/Figures/Figure_3/")

origins <- read_tsv("origin_data/origins.tsv")
params <- read_csv("origin_data/params.csv")

origins_1 <- origins %>% filter(Identity > 0) %>% group_by(SimulationID) %>% 
  filter(Identity == min(Identity)) %>% mutate(Y = 4 * sqrt((OriginX-338)^2 + (OriginY-338)^2))

df <- merge(origins_1, params, by.x = "SimulationID" , by.y = "index" )

# Define paramters
mu <- 1e-5
c_wt <- 0.153
#c_m <- 0.307
demesize <- 16
s_m <- 0.3
rho <- (1-1/(1+s_m))/(1-1/(1+s_m)^demesize)

theta_2d <- (3*c_wt/(pi*rho*mu))^(1/3)
#theta_2d <- 1.0

xaxis <- 1:2000/10
f_Y <- 2*xaxis/theta_2d^2*gammainc(1/3,xaxis^3/theta_2d^3)


df_f <- df %>% filter(mu_driver_birth==mu, s_driver_birth==s_m)

################
### Plotting ###
################

cols <- brewer.pal(12,"Paired")

legend_1 <- "simulation"
legend_2 <- "prediction"

colours <- t(c(cols[3], "transparent"))
colnames(colours) <- c(legend_1, legend_2)

lty <- t(c("solid", "blank"))
colnames(lty) <- c(legend_1, legend_2)

# ggplot() + geom_histogram(aes(x=df_f$Y, y=..density.., linetype=legend_1), 
#                           fill=cols[3], alpha=0.5, bins=15) +
#   geom_line(aes(x=xaxis, y=f_Y, linetype=legend_2), linewidth = 1.5, col=cols[3]) +
#   labs(x="population radius, *y*", y="*f<sub>Y*(*y*)") +
#   #geom_label(aes(x=c(8),y=c(0.250), label=paste("mu = ", mu, " and s = ", s_m,  sep = "") ), size=6 ) +
#   xlim(0,75) + 
#   scale_linetype_manual(values = lty) +
#   theme_bw(base_size = 25) +
#   theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
#         legend.position = c(0.8,0.8), legend.title = element_blank()) +
#   #theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), legend.position = "none", legend.title = element_blank()) +
#   theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
#         legend.position = c(0.8,0.8), legend.title = element_blank())


fig <- ggplot() + geom_histogram(aes(x=df_f$Y, y=..density.., linetype=legend_1), 
                                 fill=cols[3], alpha=0.5, bins=15) +
  geom_line(aes(x=xaxis, y=f_Y, linetype=legend_2), linewidth = 1.5, col=cols[3]) +
  labs(x="population radius, *y*", y="*f<sub>Y*(*y*)") +
  #geom_label(aes(x=c(8),y=c(0.250), label=paste("mu = ", mu, " and s = ", s_m,  sep = "") ), size=6 ) +
  xlim(0,75) + 
  scale_linetype_manual(values = lty) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(0.8,0.8), legend.title = element_blank()) #+
  #theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), legend.position = "none", legend.title = element_blank())
  
fig

a = 1.5
#ggsave("/Users/stein02/Desktop/upload/Figures/Figure_3/figures/fY_sim.png", fig, width = 4^a, height = 3^a)







