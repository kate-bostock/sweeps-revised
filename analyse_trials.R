# title: Plotting trial numbers versus birth rate.
# author: Kate Bostock


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
library("dplyr")

###################
### Import data ###
###################

### Simulation data
params_revised <- read_csv("simulation_data_revised/1000_sims/params.csv") # contains 40000 entries of index, mu_driver_birth (1e-2, 1e-3, 1e-4, 1e-5, 1e-6), s_driver_birth (0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2)
probs_revised <- read_csv("simulation_data_revised/1000_sims/sweeps-prob-descendants.csv") # contains 40000 entries of index, sweep true/false at different % levels (70, 75, 80, 90, 95, 99, 100%), more than one mutant (true/false), first mutation init time, pop radius at first mut init)
birth_rates <- read_csv("simulation_data_revised/1000_sims/birth_rates.csv") # contains 40000 entries of the birth rate of the genotype with exactly 1 driver mutation and the most descendants
trials <- read_csv("simulation_data_revised/1000_sims/trials.csv") # contains 40000 entries of the birth rate of the genotype with exactly 1 driver mutation and the most descendants

# Find the equivalent 's' value given that there is a random element applied to generate the birth rate b2 = b1(1 +s(1 - b1/m)*r)  therefore sr = (b2/b1 - 1)/(1 - b1/m)
birth_rates <- mutate(birth_rates, sr = (birth_rate_largest_clone/b1 - 1)/(1 - b1/m))

# Merge the parameters with the data dataframe (revised)
df_revised <- merge(probs_revised, params_revised, by.x = "id" , by.y = "index" )
df_revised <- merge(df_revised, birth_rates, by.x = "id" , by.y = "index" )
df_revised <- merge(df_revised, trials, by.x = "id" , by.y = "index" )
df_revised <- df_revised[, !duplicated(as.list(df_revised))]
df_revised <- df_revised %>% arrange(birth_rate_largest_clone)

# Calculate rolling average (e.g., window of k points)
window_size <- 7
df_revised <- df_revised %>%
  mutate(rolling_avg = zoo::rollmean(trials, k = 100, fill = NA))

###################
### Plotting ###
###################

g1 <- ggplot(df_revised, aes(x = birth_rate_largest_clone, y = trials)) +
  geom_point() +
  geom_line(aes(y = rolling_avg), color = "red", linewidth = 1)

g1
