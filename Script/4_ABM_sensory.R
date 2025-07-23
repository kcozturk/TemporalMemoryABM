#### SETUP R-ENVIRONMENT ####

rm(list=ls())  # Clear environment

# Load required libraries
library(circular)     
library(truncnorm)    
library(compiler)     
library(future.apply) 

setwd("/home/picocluster/Documents/ABM")  # Set working directory
source("Functions.R")  # Load external functions

plan(multicore, workers = 6)  # Enable parallel processing with 6 workers

##### PARAMETERS #####
seed <- 4
script_max_comb <- 12 * 12 * 8 # for unique combinations acros simulations determine max combinations for all scripts

nTree <- 320          # Productivity
nr_simul <- 300       # Number of simulations
homogen <- 1          # Homogeneity parameter
cent_std <- 0         # Standard deviation of centered trees
memoryused <- F       # Disable memory usage
memory_slots <- 1
inaccuracy_factor <- 0

mem_length_ts <- 120  # Memory retention time before it is forgotten
target_scale <- 0.5   # Scale determining the probability that memory is targeted
movspeed <- 500       # Movement speed of agent
eatrate <- 10         # The time in which an agent eats 1 afu. 

# Generate sequences for parameter values
time_to_dis_values <- 0.25 * 2^(0:7)

# Create all parameter combinations
param_grid <- expand.grid(time_to_dis = time_to_dis_values)
total_combinations <- nrow(param_grid)

# Initialize output matrix
output <- matrix(nrow = nr_simul, ncol = 26)
colnames(output) <- c("eaten", "n_fruits_created", "scalarproptimeval", "inaccuracy_factor",
                      "time_to_ripen", "edibilityPrim", "time_to_dis", "decay_start", 
                      "Fruit_max", "visual_det_range", "nTree", "env_xmin", "env_ymin",
                      "env_ymax", "AverageRegenSteps", "eat_range", "mem_length_ts",
                      "memory_slots", "stepsize_mean", "stepsize_sd", "wrapped_rho",
                      "time", "ts", "seed", "cent_std", "memory")

# Run simulations for each parameter combination
for (z in 1:total_combinations) {
  
  # Extract parameters for this iteration
  time_to_dis <- param_grid[z, 1]
  
  # Run simulations in parallel
  results_list <- future_lapply(1:nr_simul, function(x) {
      global_seed <- (seed - 1) * (nr_simul * script_max_comb) + (z - 1) * nr_simul + x
      set.seed(global_seed)
      ABM()
      }, future.seed = TRUE)
  
  # Convert results to array
  output <- simplify2array(results_list)
  
  # Save results to CSV
  write.csv(output, file = paste0("Output/output_sensory_decay", z, ".csv"))
}



#### SETUP R-ENVIRONMENT ####

rm(list=ls())  # Clear environment

# Load required libraries
library(circular)     
library(truncnorm)    
library(compiler)     
library(future.apply) 

setwd("/home/picocluster/Documents/ABM")  # Set working directory
source("Functions.R")  # Load external functions

plan(multicore, workers = 6)  # Enable parallel processing with 6 workers

##### PARAMETERS #####
seed <- 5
script_max_comb <- 12 * 12 * 8 # for unique combinations acros simulations determine max combinations for all scripts

time_to_dis <- 4    # Decay Period
nr_simul <- 300     # Number of simulations
homogen <- 1        # Homogeneity parameter
cent_std <- 0       # Standard deviation of centered trees
memoryused <- F     # Enable memory usage
memory_slots <- 1   
inaccuracy_factor <- 0

mem_length_ts <- 120  # Memory retention time before it is forgotten
target_scale <- 0.5 # Scale determining the probability that memory is targeted
movspeed <- 500       # Movement speed of agent
eatrate <- 10         # The time in which an agent eats 1 afu. 

# Generate sequences for parameter values
nTree_values <- 20 * 2^(0:7)

# Create all parameter combinations
param_grid <- expand.grid(nTree = nTree_values)
total_combinations <- nrow(param_grid)

# Initialize output matrix
output <- matrix(nrow = nr_simul, ncol = 26)
colnames(output) <- c("eaten", "n_fruits_created", "scalarproptimeval", "inaccuracy_factor",
                      "time_to_ripen", "edibilityPrim", "time_to_dis", "decay_start", 
                      "Fruit_max", "visual_det_range", "nTree", "env_xmin", "env_ymin",
                      "env_ymax", "AverageRegenSteps", "eat_range", "mem_length_ts",
                      "memory_slots", "stepsize_mean", "stepsize_sd", "wrapped_rho",
                      "time", "ts", "seed", "cent_std", "memory")

# Run simulations for each parameter combination
for (z in 1:total_combinations) {
  
  # Extract parameters for this iteration
  nTree <- param_grid[z, 1]
  
  # Run simulations in parallel
  results_list <- future_lapply(1:nr_simul, function(x) {
      global_seed <- (seed - 1) * (nr_simul * script_max_comb) + (z - 1) * nr_simul + x
      set.seed(global_seed)
      ABM()
      }, future.seed = TRUE)
  
  # Convert results to array
  output <- simplify2array(results_list)
  
  # Save results to CSV
  write.csv(output, file = paste0("Output/output_sensory_prod", z, ".csv"))
}



#### SETUP R-ENVIRONMENT ####

rm(list=ls())  # Clear environment

# Load required libraries
library(circular)     
library(truncnorm)    
library(compiler)     
library(future.apply) 

setwd("/home/picocluster/Documents/ABM")  # Set working directory
source("Functions.R")  # Load external functions

plan(multicore, workers = 6)  # Enable parallel processing with 6 workers

##### PARAMETERS #####
seed <- 6
script_max_comb <- 12 * 12 * 8 # for unique combinations acros simulations determine max combinations for all scripts

nTree <- 320        # Productivity
time_to_dis <- 4    # Decay Period
nr_simul <- 300     # Number of simulations
homogen <- 0        # Homogeneity parameter
num_cent <- 10      # Number of tree clusters
memoryused <- F     # Enable memory usage
memory_slots <- 1     
inaccuracy_factor <- 0

mem_length_ts <- 120  # Memory retention time before it is forgotten
target_scale <- 0.5 # Scale determining the probability that memory is targeted
movspeed <- 500       # Movement speed of agent
eatrate <- 10         # The time in which an agent eats 1 afu. 

# Generate sequences for parameter values
cent_std_values <- 2.5 * 2^(0:7)

# Create all parameter combinations
param_grid <- expand.grid(cent_std = cent_std_values)
total_combinations <- nrow(param_grid)

# Initialize output matrix
output <- matrix(nrow = nr_simul, ncol = 26)
colnames(output) <- c("eaten", "n_fruits_created", "scalarproptimeval", "inaccuracy_factor",
                      "time_to_ripen", "edibilityPrim", "time_to_dis", "decay_start", 
                      "Fruit_max", "visual_det_range", "nTree", "env_xmin", "env_ymin",
                      "env_ymax", "AverageRegenSteps", "eat_range", "mem_length_ts",
                      "memory_slots", "stepsize_mean", "stepsize_sd", "wrapped_rho",
                      "time", "ts", "seed", "cent_std", "memory")

# Run simulations for each parameter combination
for (z in 1:total_combinations) {
  
  # Extract parameters for this iteration
  cent_std <- param_grid[z, 1]
  
  # Run simulations in parallel
  results_list <- future_lapply(1:nr_simul, function(x) {
      global_seed <- (seed - 1) * (nr_simul * script_max_comb) + (z - 1) * nr_simul + x
      set.seed(global_seed)
      ABM()
      }, future.seed = TRUE)
  
  # Convert results to array
  output <- simplify2array(results_list)
  
  # Save results to CSV
  write.csv(output, file = paste0("Output/output_sensory_hetero", z, ".csv"))
}
