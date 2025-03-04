Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

summary_df <- data.frame(
  index=character(),
  drivers_end_mean=character(),
  stringsAsFactors=FALSE
) 

setwd("/Volumes/Seagate Backup Plus Drive/outputs/Revised_simulations_1000/simulations")

get_drivers <- function(configid, path){
  
  # To extract summary of birth rates from a batch of simulation outputs
  output_driver_genotype <- read_delim(
    paste(
      path,
      "/output_driver_genotype_properties.dat",
      sep=""
    ),
    "\t", escape_double = FALSE,
    trim_ws = TRUE,
    col_types = cols()
  )
  
  output_driver_genotype <- mutate(output_driver_genotype, Pop_x_dr = Population * DriverMutations)
  
  return(
    list(
      "drivers_end_mean"=sum(output_driver_genotype$Pop_x_dr)/sum(output_driver_genotype$Population)
    )
  )
}

simulation_paths <- Sys.glob(file.path("/Volumes/Seagate Backup Plus Drive/outputs/Revised_simulations_1000/simulations", "*"))
simulation_ids <- sapply(strsplit(simulation_paths, "/"), tail, 1)

for (configid in simulation_ids){
  
  returnlist = get_drivers(
    configid,
    paste(
      "",
      configid,
      sep=""
    )
  )
  
  
  drivers_end_mean = returnlist$drivers_end_mean
  
  
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, drivers_end_mean 
  )
}

write.csv(
  summary_df,
  "/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/Figure 3/simulation_data_revised/1000_sims/drivers_end_mean_all.csv"
)

