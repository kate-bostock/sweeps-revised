Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
suppressPackageStartupMessages(suppressWarnings(library(glue)))

summary_df <- data.frame(
  index=character(),
  birth_rate_max_genotype_cellweighted_mean=character(),
  birth_rate_max_genotype_mean=character(),
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
  
  # find the genotype identity with the most descendants other than WT
  output_driver_genotype_filtered <- filter(output_driver_genotype, DriverMutations == 1)  
  output_driver_genotype_filtered <- filter(output_driver_genotype_filtered, Descendants == max(output_driver_genotype_filtered$Descendants))
   
  genotype_max_descendants_identity <- output_driver_genotype_filtered$Identity[1]
  genotype_descendants_list <- c(genotype_max_descendants_identity)
  checked_genotype_list <- c()
  
  # iterate to create a list of all descendant identities
  for (i in c(1:n_distinct(output_driver_genotype$Parent))){
    temp_list <-  setdiff(genotype_descendants_list, checked_genotype_list)                                #remove the genotypes already checked from the list
      for (g in temp_list){
      output_driver_genotype_filtered <- filter(output_driver_genotype, Parent == g)                       #filter the data for entries who's parent genotype is the one being checked
      checked_genotype_list <- c(checked_genotype_list, g)                                                 #add the genotype just checked to the list of done ones so we don't repeat ourselves
      genotype_descendants_list <- c(genotype_descendants_list, output_driver_genotype_filtered$Identity)  #add the new descendants to the list of descendants
      genotype_descendants_list <- unique(genotype_descendants_list)                                       #make sure the list is unique
      genotype_descendants_list <- na.omit(genotype_descendants_list)                                      #make sure there aren't any NAs
    }
  }
  
  # filter for the max genotype and it's descendants
  output_driver_genotype_filtered <- filter(output_driver_genotype, Identity %in% genotype_descendants_list)    #filter the data for all descendants in the list created for our max first common ancestor
  output_driver_genotype_filtered <- mutate(output_driver_genotype_filtered, cells_x_BR = Population*BirthRate) #calculate the results needed to find the weighted mean
  print(paste(configid, mean(output_driver_genotype_filtered$BirthRate)))
  
  return(
    list(
      "birth_rate_max_genotype_cellweighted_mean"=sum(output_driver_genotype_filtered$cells_x_BR) / sum(output_driver_genotype_filtered$Population), 
      "birth_rate_max_genotype_mean" = mean(output_driver_genotype_filtered$BirthRate)
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
  
  
  birth_rate_max_genotype_cellweighted_mean = returnlist$birth_rate_max_genotype_cellweighted_mean
  birth_rate_max_genotype_mean = returnlist$birth_rate_max_genotype_mean
  
  
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, birth_rate_max_genotype_cellweighted_mean, birth_rate_max_genotype_mean 
  )
}

write.csv(
  summary_df,
  "/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/outputs/Revised_simulations_1000/birth_rate_max_genotype.csv"
)

