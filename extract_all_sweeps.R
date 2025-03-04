Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

summary_df <- data.frame(
  id=character(),
  sweep_init_time=character(),
  pop_radius_at_sweep_init=character(),
  stringsAsFactors=FALSE
) 

sweep_extraction <- function(configid, path){
  
  # Collect times and locations of all 100% sweeps:
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
  
  output_driver_genotype_properties <- read_delim(
    paste(
      path,
      "/output_driver_genotype_properties.dat",
      sep=""
    ),
    "\t", escape_double = FALSE,
    trim_ws = TRUE,
    col_types = cols()
  )
  
  output_driver_genotype_properties <- mutate(
    output_driver_genotype_properties,
    Frequency = Descendants / max(Descendants)
 )
  
  sweep_table <- filter(output_driver_genotype_properties, output_driver_genotype_properties$Frequency >= 1.0)
  sweep_table <- subset(sweep_table, select = c("OriginTime"))
  sweep_table <- cbind(rep(configid, nrow(sweep_table)), sweep_table, rep(0, nrow(sweep_table)))
  names(sweep_table) <- c("id","sweep_init_time", "pop_radius_at_sweep_init" )
  
  for (i in c(1:nrow(sweep_table))){
    if (sweep_table$sweep_init_time[i] != 0){
    output_filtered <- filter(output, abs(Generation - sweep_table$sweep_init_time[i]) == min(abs(Generation - sweep_table$sweep_init_time[i])))
    pop_at_sweep_init <- output_filtered$NumCells
    sweep_table$pop_radius_at_sweep_init[i] = sqrt(pop_at_sweep_init / pi)
    }
  }

  
  return(
    list(
      "sweep_table"=sweep_table
    )
  )
}

simulation_paths <- Sys.glob(file.path("/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/outputs/Revised_simulations_1000/simulations/", "*"))
simulation_ids <- sapply(strsplit(simulation_paths, "/"), tail, 1)

for (configid in simulation_ids){
  
  returnlist = sweep_extraction(
    configid,
    paste(
      "",
      configid,
      sep=""
    )
  )
  
  sweep_table = returnlist$sweep_table
  summary_df<- rbind(summary_df, sweep_table)

}

write.csv(
  summary_df,
  "sweeps-all.csv", row.names = FALSE
)
