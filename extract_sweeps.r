Sys.setenv("LANGUAGE"="EN")

suppressPackageStartupMessages(suppressWarnings(library(readr)))
#suppressPackageStartupMessages(suppressWarnings(library(demonanalysis)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))

summary_df <- data.frame(
  id=character(),
  x070=character(),
  x075=character(),
  x080=character(),
  x085=character(),
  x090=character(),
  x095=character(),
  x099=character(),
  x100=character(),
  more_than_one_mutant=character(),
  first_mutant_init_time=character(),
  pop_radius_at_first_mut_init=character(),
  stringsAsFactors=FALSE
) 

sweep <- function(df, x) ifelse(max(df$Frequency) >= x, TRUE, FALSE)

test_for_a_sweep <- function(configid, path){

  # To test whether more than one mutant is present at the end of the simulation
  # (in which case a sweep would be unlikely to occur if we kept the simulation running):
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

  more_then_one_mutant <- TRUE #filter(
 #   output,
 #   NumCells == max(NumCells)) %>% summarise(NumGenotypes > 2 || (NumGenotypes > 1 && CellsWith0Drivers == 0)
 # ) %>% as.logical()
  
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
   # Frequency = Population / max(Population) # changed to Descendants as populations <> to descendants in the revised sweeps paper.
  )

  # test for a sweep with a range of thresholds
  sweep_marker <- sapply(
    c(0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.99, 1.0),
    sweep, df = filter(output_driver_genotype_properties, Identity > 0)
  )

  # To find the time when the first mutant arose:
  RobustMin <- function(x) {if (length(x)>0) min(x) else NA}
                            
  first_mutant_init_time <- output_driver_genotype_properties %>%
    filter(DriverMutations > 0) %>% summarise(RobustMin(OriginTime)) %>% as.numeric()

  pop_at_first_mut_init <- output %>% 
    filter(abs(Generation - first_mutant_init_time) == min(abs(Generation - first_mutant_init_time))) %>%
    select(NumCells) %>% slice(1)
  # The first line ends with slice(1) just in case two rows have the same Generation value
  # (though I don't think this will ever happen). ~Rob

  pop_radius_at_first_mut_init = sqrt(pop_at_first_mut_init / pi)

  return(
    list(
      "sweep_marker"=sweep_marker,
      "more_then_one_mutant"=more_then_one_mutant,
      "first_mutant_init_time"=first_mutant_init_time,
      "pop_radius_at_first_mut_init"=pop_radius_at_first_mut_init
    )
  )
}

simulation_paths <- Sys.glob(file.path("/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/Figure 3/output_sweeps_small_s/simulations", "*"))
simulation_ids <- sapply(strsplit(simulation_paths, "/"), tail, 1)

for (configid in simulation_ids){

  returnlist = test_for_a_sweep(
    configid,
    paste(
      "",
      configid,
      sep=""
    )
  )
    
  sweep_marker = returnlist$sweep_marker
  more_then_one_mutant = returnlist$more_then_one_mutant
  first_mutant_init_time = returnlist$first_mutant_init_time
  pop_radius_at_first_mut_init = returnlist$pop_radius_at_first_mut_init
  summary_df[nrow(summary_df) + 1,] <- c(
    configid, sweep_marker, more_then_one_mutant, first_mutant_init_time, pop_radius_at_first_mut_init
  )
}

write.csv(
  summary_df,
  "sweeps-prob-descendants.csv"
)
