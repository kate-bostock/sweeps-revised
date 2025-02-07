# title: Plotting sweep probabilities versus exact fitness of sweep ancestor.
# author: Alexander Stein, Kate Bostock

#################
### libraries ###
#################

library("tidyverse")
library("ggtext")
library("RColorBrewer")
library("ggplot2")

### choose graph settings ###
sweep_cutoff <- "x070" # options are: x070, x075, x080, x085, x090, x095, x099, x100

###################
### Import data ###
###################

### Simulation data
params_revised <- read_csv("simulation_data_low_birth_rates/params.csv") # contains 40000 entries of index, mu_driver_birth (1e-2, 1e-3, 1e-4, 1e-5, 1e-6), s_driver_birth (0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2)
probs_revised <- read_csv("simulation_data_low_birth_rates/sweeps-prob-descendants.csv") # contains 40000 entries of index, sweep true/false at different % levels (70, 75, 80, 90, 95, 99, 100%), more than one mutant (true/false), first mutation init time, pop radius at first mut init)
birth_rates <- read_csv("simulation_data_low_birth_rates/birth_rates.csv") # contains 40000 entries of the birth rate of the genotype with exactly 1 driver mutation and the most descendants


# Merge the parameters with the data dataframe (revised)
df_revised <- merge(probs_revised, params_revised, by.x = "id" , by.y = "index" )
df_revised <- merge(df_revised, birth_rates, by.x = "id" , by.y = "index" )

s_vector = c(0.0001,0.001,0.01,0.1) # list of s_driver_birth rates that were used in the 40,000 batch of simulations for which the parameters are in params.csv

y_data1 <- c(0,0,0,0)
i1=1
for (s in s_vector) {
  df_ff <- df_revised %>% filter(s_driver_birth == s)  # filter the original 40k simulations for the relevant mutation rate, to each s_driver_birth
  logical <- select(df_ff,sweep_cutoff)                           # filter for those which did have a sweep pass the 100% level
  prob <- sum(logical)/1000                       # count how many had a sweep and divide by 1000 which is 40000/number of different mutation rates/number of different birth rates. i.e. the total number of sims that should have been filtered down to at the previous filter before the sweeps filter.
  y_data1[i1] <- prob                             # store the % in the y_data file
  i1 <- i1+1
}


#############
### Plots ###
#############
my_data <- data.frame(x=s_vector, y=y_data1)

g1 <- ggplot(my_data, aes(x = x, y = y)) +
  geom_point()

g1
