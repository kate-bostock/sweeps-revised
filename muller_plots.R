library(demonanalysis)

path <- "/Users/katebostock/Documents/City_PhD/demon_model/Sweeps_revised/Figure 3/simulation_data_revised/1000_sims/muller_plots/simulations/00001DA3" # insert name of top-level folder

# input_dir <- paste0(subfolder_name) # folder containing results of the batch
# num_parameters <- count_parameters(input_dir) # number of simulation parameters (first columns in data)
# output_dir_plots <- paste0(subfolder_name) # folder to receive image files
# output_dir_data <- paste0(subfolder_name) # folder containing data files

#create_plots_batch(input_dir, output_dir = output_dir_plots, type = "plot")

#plot_all_images(input_dir, output_dir_plots)

plot_all_images(path, output_filename = "muller", file_type = "png", output_dir = path, trim = -1, include_genotype_plots = TRUE, 
                            cutoff = 0, min_birth_rate = NA, max_birth_rate = NA) 