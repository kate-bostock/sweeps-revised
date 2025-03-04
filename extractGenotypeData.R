Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

summary_df <- data.frame(
  index=character(),
  EverGenotypes=character(),
  maxCells= character(),
  maxGeneration = character(),
  stringsAsFactors=FALSE
) 

#setwd("/Volumes/Seagate Backup Plus Drive/Revised_simulations_1000_2mutations/simulations")

get_genotype_data <- function(configid, path){
  
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
  
  
  
  return(
    list(
      "EverGenotypes"=max(output$EverGenotypes),
      "maxCells"=max(output$NumCells),
      "maxGeneration" =max(output$Generation) 
  ))
}

simulation_paths <- Sys.glob(file.path("/Volumes/Seagate Backup Plus Drive/Revised_simulations_1000_2mutations/simulations", "*"))
simulation_ids <- sapply(strsplit(simulation_paths, "/"), tail, 1)

for (configid in simulation_ids){
  
  returnlist = get_genotype_data(
    configid,
    paste(
      "",
      configid,
      sep=""
    )
  )
  
  
  EverGenotypes = returnlist$EverGenotypes
  maxCells = returnlist$maxCells
  maxGeneration = returnlist$maxGeneration
  
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, EverGenotypes, maxCells, maxGeneration 
  )
}

write.csv(
  summary_df,
  "EverGenotypes.csv"
)