#################
### libraries ###
#################
library("tidyverse")
library("RColorBrewer")

###################
### Import data ###
###################

### Simulation data
params <- read_csv("simulation_data_revised/1000_sims/params.csv") 
probs <- read_csv("simulation_data_revised/1000_sims/sweeps-prob-descendants.csv")
drivers <- read_csv("simulation_data_revised/1000_sims/drivers_end_mean_all.csv")
s_vector = c(0.1,0.2,0.3,0.4,0.5,1,1.5,2)

# Merge the parameters with the data dataframe
df <- merge(probs, params, by.x = "id" , by.y = "index" )

# merge with drivers data
df2 <- merge(df, drivers, by.x = "id",by.y = "index" )

###################
#### Analysis #####
###################

 # find mean number of drivers per simulation for the different s values.
 means_list <-c()
 for (s in s_vector)
   means_list <- c(means_list, mean(filter(df2, s_driver_birth == s)$drivers_end_mean))

###################
#### Plotting #####
###################

 ggplot() + 
   geom_line(aes(x = s_vector, y = means_list)) +
   geom_point(aes(x = s_vector, y = means_list))

ggplot(df2, aes(x = drivers_end_mean)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  xlim(0,9) +
  facet_wrap(~ s_driver_birth) +  # Facet by  the 's_driver_birth' column
  labs(title = "Histograms of mean drivers at sim end by s_driver_birth", x = "No. driver mutations", y = "No. Sims")