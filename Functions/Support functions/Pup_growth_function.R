# Pup growth rate function
# Used to calculate the cost of growth to the pup

# This is a source function for the Pup_energy_function

library(here)
source(here("Functions/Support functions/vonBertalanffy_growth_function.R"))

pup_growth_function <- function(t = 1){
  # d = 1/365 
  # mass_added = VBGF(a = d*t) - VBGF(a = d*(t-1))
  mass_added = 0.06*t
  mass_added
} 

prop_lipid_pup <- function(m){
  (0.7498 * m + 6.8696)/100
  }

prop_protein_pup <- function(m){
  (0.1375 * m + 22.489)/100
}