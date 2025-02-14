Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

summary_df <- data.frame(
  index=character(),
  birth_rate_last_sweep=character(),
  stringsAsFactors=FALSE
) 

setwd("/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/outputs/Revised_simulations_1000/simulations")

get_birth_rate <- function(configid, path){
  
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
  
  output_driver_genotype <- filter(output_driver_genotype, DriverMutations != 0)  
  output_driver_genotype <- filter(output_driver_genotype, Descendants == max(output_driver_genotype$Descendants))
  output_driver_genotype <- filter(output_driver_genotype, DriverMutations == max(output_driver_genotype$DriverMutations))  

    return(
    list(
      "birth_rate_last_sweep"=mean(output_driver_genotype$BirthRate)
    )
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
  
  
  birth_rate_last_sweep = returnlist$birth_rate_last_sweep

  
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, birth_rate_last_sweep 
  )
}

write.csv(
  summary_df,
  "/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/outputs/Revised_simulations_1000/birth_rates_finalsweep.csv"
)

