Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

summary_df <- data.frame(
  index=character(),
  mu_driver_birth=character(),
  s_driver_birth=character(),
  stringsAsFactors=FALSE
) 


get_parameters <- function(configid, path){

  # To extract summary of mu_driver_birth and s_driver_birth rates from a batch of simulations
  parameters <- read_delim(
    paste(
      path,
      "/parameters.dat",
      sep=""
    ),
    "\t", escape_double = FALSE,
    trim_ws = TRUE,
    col_types = cols()
  )

  return(
    list(
      "mu_driver_birth"=parameters$mu_driver_birth,
      "s_driver_birth"=parameters$s_driver_birth
    )
  )
}

simulation_paths <- Sys.glob(file.path("/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/outputs/outputsrevised_low_birth_rate/simulations", "*"))
simulation_ids <- sapply(strsplit(simulation_paths, "/"), tail, 1)

for (configid in simulation_ids){

  returnlist = get_parameters(
    configid,
    paste(
      "",
      configid,
      sep=""
    )
  )
    
  
  mu_driver_birth = returnlist$mu_driver_birth
  s_driver_birth = returnlist$s_driver_birth
  
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, mu_driver_birth, s_driver_birth
  )
}

write.csv(
  summary_df,
  "params.csv"
)

