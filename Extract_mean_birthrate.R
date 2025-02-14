#### Extract weighted average birth rate of all mutated cells ####
# need the time period for each generation, so Generation_time = Generation current less previous, or just Generation for first row
# number of cells with mutations = NumCells - CellsWith0Drivers
# mean birth rate of mutated cells = (MeanBirthRate * NumCells - CellsWith0Drivers) /NumCells_withdrivers
# weight the mean birth rates: weighted mean birth rate = mean birth rate of mutated cells * NumCells_withdrivers * Generation_time
# denominator : = NumCells_withdrivers * Generation_time
# total average = sum(weighted mean birth rate) / sum(denominator)



Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

summary_df <- data.frame(
  index=character(),
  birth_rate_mean=character(),
  stringsAsFactors=FALSE
) 

setwd("/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/outputs/Revised_simulations_1000/simulations")

get_birth_rate <- function(configid, path){
  
  # To extract summary of birth rates from a batch of simulation outputs
  output <- read_delim(
    paste(
      path,
      "/output.dat",
      sep=""
    ),
    "\t", escape_double = FALSE,
    trim_ws = TRUE,
    col_types = cols()
  )
  
  # Calculate the differences between consecutive timestamps
  interval_time <- c(output$Generation[1], diff(output$Generation))
  
  # Add the interval_time as a new column to the dataframe
  output$interval_time <- interval_time
  
  # # number of cells with drivers = NumCells - CellsWith0Drivers
  # output <- mutate(output, NumCells_withDrivers = NumCells - CellsWith0Drivers)
  # 
  # # mean birth rate of mutated cells = (MeanBirthRate * NumCells - CellsWith0Drivers) /NumCells_withdrivers
  # output <- mutate(output, mean_br = ifelse(NumCells_withDrivers ==0, 0, (MeanBirthRate * NumCells - CellsWith0Drivers)/NumCells_withDrivers))
  # 
  # # weight the mean birth rates: weighted mean birth rate = mean birth rate of mutated cells * NumCells_withdrivers * Generation_time
  # output <- mutate(output, numerator = mean_br * NumCells_withDrivers * interval_time)
  # 
  # # denominator : = NumCells_withdrivers * Generation_time
  # output <- mutate(output, denominator = NumCells_withDrivers * interval_time)
  # 
  # # total average = sum(weighted mean birth rate) / sum(denominator)  
  # birth_rate_mean = sum(output$numerator)/sum(output$denominator)
  
  # try mean over all cells
  output <- mutate(output, numerator = NumCells * interval_time * MeanBirthRate)
  output <- mutate(output, denominator = NumCells * interval_time)
  
  # total average = sum(weighted mean birth rate) / sum(denominator)  
  birth_rate_mean = sum(output$numerator)/sum(output$denominator)
  
  
  return(
    list("birth_rate_mean"= birth_rate_mean)
  )
}

simulation_paths <- Sys.glob(file.path("/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/outputs/Revised_simulations_1000/simulations", "*"))
simulation_ids <- sapply(strsplit(simulation_paths, "/"), tail, 1)

for (configid in simulation_ids){
  
  returnlist = get_birth_rate(
    configid,
    paste(
      "",
      configid,
      sep=""
    )
  )
  
  
  birth_rate_mean = returnlist$birth_rate_mean
  
  
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, birth_rate_mean 
  )
}

write.csv(
  summary_df,
  "/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/outputs/Revised_simulations_1000/birth_rates_mean2.csv"
)

