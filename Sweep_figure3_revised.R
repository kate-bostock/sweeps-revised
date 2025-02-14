# title: Plotting sweep probabilities versus exact fitness of sweep ancestor.
# author: Alexander Stein, Kate Bostock


# Get data for each simulation
# filter data for mutants with exactly one driver mutation and find the one with the greatest % population for it's descendants
# add extra column to sweeps_prob file or similar.
# then extract fitness value to plot on the modified x-axis in bins... 
# largest is the closest to a sweep, want to understand the effect of the first mutation fitness...

#################
### libraries ###
#################

library("tidyverse")
library("ggtext")
library("RColorBrewer")

### choose graph settings ###
sweep_cutoff <- "x100" # options are: x070, x075, x080, x085, x090, x095, x099, x100

### assumptions ###

m <- 10 # maximum birth rate
b1 <- 1 # base birth rate

###################
### Import data ###
###################

### Simulation data
params <- read_csv("simulation_data_original/params.csv") # contains 40000 entries of index, mu_driver_birth (1e-2, 1e-3, 1e-4, 1e-5, 1e-6), s_driver_birth (0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2)
probs <- read_csv("simulation_data_original/sweeps-prob.csv") # contains 40000 entries of index, sweep true/false at different % levels (70, 75, 80, 90, 95, 99, 100%), more than one mutant (true/false), first mutation init time, pop radius at first mut init)

param_wt <- read_csv("simulation_data_original/params_wt.csv") # contains 2560 entries of log2_deme (1, 2, 3, 4, 5, 6, 7, 8), migration_rate (0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 1), normal_birth rate (0.95238095, 0.90909091, 0.833333333, 0.71428571)
meas_wt <- read_csv("simulation_data_original/wt_speeds.csv") # contains 2560 entries of speed_early, speed_late

param_mut <- read_csv("simulation_data_original/params_mut.csv") # contains 12800 entries of log2_deme(1, 2, 3, 4, 5, 6, 7, 8), migration_rate (0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 1), s_driver_birth (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2)
meas_mut <- read_csv("simulation_data_original/mut_speeds.csv") # contains 12800 entries of speed_early, speed_late

params_revised <- read_csv("simulation_data_revised/1000_sims/params.csv") 
probs_revised <- read_csv("simulation_data_revised/1000_sims/sweeps-prob-descendants.csv") 
birth_rates_mean <- read_csv("simulation_data_revised/1000_sims/birth_rates_end_mean.csv") 

# Numerical solutions
df_num <- read_csv("numerical_data/sweep probability vs speed ratio c_wt 0.152 mutation rate 2.34e-06 simplifying assumption 0.csv")
df_num <- rename(df_num, c("ratio"="Speed ratio", "twodim"="2D", "threedim"="3D"))

# Some converting things
df_num$twodim <- as.complex(df_num$twodim)
df_num$twodim <- Re(df_num$twodim)

df_num$threedim <- as.complex(df_num$threedim)
df_num$threedim <- Re(df_num$threedim)

# Find the equivalent 's' value given that there is a random element applied to generate the birth rate b2 = b1(1 +s(1 - b1/m)*r)  therefore sr = (b2/b1 - 1)/(1 - b1/m)
birth_rates_mean <- mutate(birth_rates_mean, sr1 = (birth_rate_end_mean/b1 - 1)/(1 - b1/m))


# Merge the parameters with the data dataframe (orginal)
df <- merge(probs, params, by.x = "id" , by.y = "index" )
df_wt <- merge(meas_wt, param_wt, by.x = "id" , by.y = "index" )
df_mut <- merge(meas_mut, param_mut, by.x = "id" , by.y = "index" )

# Merge the parameters with the data dataframe (revised)
df_revised <- merge(probs_revised, params_revised, by.x = "id" , by.y = "index" )
df_revised <- merge(df_revised, birth_rates_mean, by.x = "id" , by.y = "index" )
df_revised <- df_revised[, !duplicated(as.list(df_revised))]

###########################
### Read out the speeds ###
###########################

# Filter data of interest
df_wt_f <- df_wt %>% filter(log2_deme==4, migration_rate==0.05, 
                            normal_birth_rate>=0.9, normal_birth_rate<=0.95) # in the base case just gives us the 10 entries with the birth rate 0.90909090
df_mut_f <- df_mut %>% filter(log2_deme==4, migration_rate==0.05) # gives 200 entries; 10 simulations for each of 20 different s_driver_birth rates

c_wt <- mean(df_wt_f$speed_late) # just one single value of the average 'speed_late' of all the different s_driver_birth rates
#std_wt <- sd(df_wt_f$speed_late)

#c_mut <- c(0,0,0,0,0,0,0,0)
#s_vector = c(0.1,0.2,0.3,0.4,0.5,1.0,1.5,2.0) # (defined again below) list of s_driver_birth rates that were used in the 40,000 batch of simulations for which the parameters are in params.csv
# j=1
# for (s in s_vector) {
#   df_mut_ff <- df_mut_f %>% filter(s_driver_birth==s) # for each s_driver_birth rate we get 10 filtered entries -> don't think this is then used
#   #print(s)
#   #c_mut[j] <- mean(df_mut_ff$speed_late)
#   j <- j+1
# }

# Prepare the speed-selection correspondence
s_xaxis = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,
            1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0) # the list of s_driver_birth values in the params_mut file
c_xaxis = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,
            1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0) # the list of s_driver_birth values in the params_mut file
j=1
for (s in s_xaxis) {
  df_mut_ff <- df_mut_f %>% filter(s_driver_birth==s) # 10 entries for each s, 
  #print(s)
  c_xaxis[j] <- mean(df_mut_ff$speed_late) # take the mean of the 10 simulation runs for each s_driver_birth rate.
  j <- j+1
}

####################
### Computations ###
####################

### Define parameters of the simulation
#pi <- 3.1415
#mig <- 0.05
s_wt <- 0.1 # NOT CLEAR WHAT THIS IS YET (maybe to rescale based on bottom level of the mutant proliferation rate a_m (s_driver_birth rate))
#demesize <- 16

### Compute Fisher speed of the wildtype
#c_Fisher <- 2*sqrt(s_wt*(mig*demesize)/2)

s_vector = c(0.1,0.2,0.3,0.4,0.5,1.0,1.5,2.0) # list of s_driver_birth rates that were used in the 40,000 batch of simulations for which the parameters are in params.csv
#mu_vector = c(1e-06,1e-05,1e-04,1e-03,1e-02)

### Filter data of interest
#df_f1 <- df %>% filter(mu_driver_birth == 1e-04)
df_f2 <- df %>% filter(mu_driver_birth == 1e-05) # filter the summary outputs from the 40k simulation batch for those with this mutation rate
#df_f3 <- df %>% filter(mu_driver_birth == 1e-06)

### Compute analytic results
#speeds <- df_ff$pop_radius_at_first_mut_init/df_ff$first_mutant_init_time
#c_wt_data <- mean(speeds)
#c_m <- 2*sqrt(s_vector*(mig*demesize)/2)

### Compute analytic sweep probability
#c_m_art <- 100:2000/1000
#xaxis <- c_m_art/c_wt
#sweep_ana <- ((c_m_art-c_wt)/c_m_art)^2

xaxis <- s_xaxis/s_wt # divide the list of s_driver_birth values in the params_mut file by s_wt which is 0.1 (possibly to rescale?)
sweep_ana <- ((c_xaxis-c_wt)/c_xaxis)^2 # This is the paper result of Pr(sweep) approx in 2 dimensions = ((c_m-c_wt)/c_m)^2. 
# c_wt is the average speed of the wildtype cells across the wt batch of simulations
# c_xaxis is the mean of the mutant cell speed at each of the different s_driver_birth rates


# Compute the sweep probabilities from data
x_data <- s_vector/s_wt #list of s_driver_birth rates that were used in the 40,000 batch of simulations divided by 0.1

y_data1 <- c(0,0,0,0,0,0,0,0)
i1=1
for (s in s_vector) {
  df_ff <- df_revised %>% filter(s_driver_birth == s)
  logical <- df_ff$x100
  prob <- sum(logical)/1000
  y_data1[i1] <- prob
  i1 <- i1+1
}

y_data2 <- c(0,0,0,0,0,0,0,0)
i2=1
for (s in s_vector) {
  df_ff <- df_f2 %>% filter(s_driver_birth == s)  # filter the original 40k simulations for the relevant mutation rate, to each s_driver_birth
  logical <- select(df_ff,sweep_cutoff)                           # filter for those which did have a sweep pass the 100% level
  prob <- sum(logical)/1000                       # count how many had a sweep and divide by 1000 which is 40000/number of different mutation rates/number of different birth rates. i.e. the total number of sims that should have been filtered down to at the previous filter before the sweeps filter.
  y_data2[i2] <- prob                             # store the % in the y_data file
  i2 <- i2+1
}

y_data3 <- c(0,0,0,0,0,0,0,0)                     # process the revised simulation data
i3=1

b_bins <- 2*(2^seq(0, log2(6), length = 40) -1) # the bins in to which the birth rates of the sweeps first mutant will be sorted
b_bins <- b_bins[-1]
b_bins_bottom <- c(0,head(b_bins, -1))
b_bins_middle <- (b_bins + b_bins_bottom ) / 2
b_bins_plotting <- b_bins_middle / s_wt
b_bins_width <- (b_bins - b_bins_bottom)/s_wt
previous_b <- 0

for (b in b_bins) {
  df_ff <- df_revised %>% filter(sr1 > previous_b, sr1 <= b)
  logical <- select(df_ff,sweep_cutoff)
  prob <- sum(logical)/nrow(df_ff)
  y_data3[i3] <- prob
  i3 <- i3+1
  previous_b <- b
}

#fix <- (1-1/(1+s_vector))/(1-1/(1+s_vector)^16)

#############
### Plots ###
#############

cols <- brewer.pal(12,"Paired")

legend_1 <- "simulation"
legend_2 <- "prediction (exact)"
legend_3 <- "prediction (approx.)"

colours <- t(c(cols[4],cols[3]))
colnames(colours) <- c(legend_2, legend_3)
shapes <- t(c(16,17,18))

lty <- t(c("22", "solid"))
colnames(lty) <- c(legend_2, legend_3)

ggplot() +
  geom_bar(aes(x=b_bins_plotting, y=y_data3), stat='identity', colour = "darkgrey", fill = "darkslategray2", position = "dodge", width = b_bins_width) +
  geom_line(aes(x=2:20, y=df_num$twodim, linetype=legend_2, color=legend_2), size=1.5) +
  geom_line(aes(x=xaxis, y=sweep_ana,linetype=legend_3, color=legend_3), linewidth=1.5) +
  #geom_point(aes(x=x_data, y=y_data1), color="black", size=4.0, shape=16) +
  geom_point(aes(x=x_data, y=y_data2), color="black", size=4.0, shape=17) +
  #geom_point(aes(x=b_bins_plotting, y=y_data3), color="blue", size=4.0, shape=18) +
  #geom_line(aes(x=x_data, y=fix),col = "black", linewidth=2.0) +
  labs(x="ratio of fitness difference, *a<sub>m* / *a<sub>wt*", y="Pr(sweep)") +
  xlim(0,100) + ylim(0,1) +
  scale_color_manual(values = colours) +
  scale_linetype_manual(values = lty) +
  scale_shape_manual(values = shapes) +
  theme_bw(base_size = 25) +
  theme(axis.title.x = element_markdown(), axis.title.y = element_markdown(), 
        legend.position = c(5,0.8), legend.title = element_blank(), legend.text = element_markdown()) 
  #ggtitle(paste0("Sweeps cut off at ", sweep_cutoff))


# ### Save the figure

# a = 1.5
# #ggsave("/Users/stein02/Desktop/Plots for the Sweeps/final figures/fig5Ball2.png", fig5B2, width = 4^a, height = 3^a)
# fig5B2
# 
# 
