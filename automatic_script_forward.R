# Code used to automate the forward simulations for
# Speakman et al 2024 Vulnerability to fluctuations in prey and predation landscapes 
# in a central place foraging marine predator

# Model developed for Australians fur seals by CN Speakman

# Load required libraries:
library(here)
library(dplyr)
library(tidyverse)

# Load backward simulation data:
# load(here("Outputs/Backward_simulations/backward_mortality_base_prey_base.RData"))
rm(list=setdiff(ls(), c("V", "L", "R"))) # retain only relevant data

# Set forward simulation settings:
startT = 1 # Start time
endT = (365*11)+1 # End time; +1 included to allow for births at the end of the final year
offset = 0 # Time offset; used for longer term simulations
N_init = 500 # Initial population size
Max_inds = 3000 # Maximum individuals for simulation
part = 1 # Simulation part; used for longer term simulations
yearsCompletedTotal = 0 # Years completed total; used for longer term simulations
reps = 10 # Number of repetitions for the simulation

for(m in 1:3){ # Mortality scenario
  if(m == 1){
    mortality_scenario = "base"
  } else if(m == 2){
    mortality_scenario = "ten"
  } else if(m == 3){
    mortality_scenario = "twenty"
  }
  
  for(p in 1:3){ # Probability of capturing food
    if(p == 1){
      prob_food = 0.85
    } else if(p == 2){
      prob_food = 0.75
    } else if(p == 3){
      prob_food = 0.95
    } 
    
    for(f in c(2,4,6,8)){ # Number of productive foraging areas encountered
      productive_areas = f
      
      source(here("Functions/Load_functions_forwards.R"))
      scenario = nameScenario(m, p, f)
      
      for(n in 1:reps){
        N_inds = N_init
        
        # Run the forward simulation for the scenario:
        source(here("run_forward_simulation.R"))
        source(here("create_all_inds.R"))
        
        # Save outputs:
        # save(Ind, file = here(paste0("Outputs/Forward_simulations/Raw_data/", scenario, "_rep_", n, ".RData")))
        # save(all_inds, file = here(paste0("Outputs/Forward_simulations/Raw_data/", scenario, "_rep_", n, "_all_inds.RData")))
        
        # Run source codes to output plots and summary data:
        # source(here("Plotting code/Summary_code_forward.R"))
      }
      
      # Clear environment:
      rm(list=setdiff(ls(), c("V", "L", "R", "part", "yearsCompletedTotal", "offset", "startT", "endT", "Max_inds", 
                              "m", "p", "f", "n", "N_init", "reps", "mortality_scenario", "prob_food")))
    }
  }
}
