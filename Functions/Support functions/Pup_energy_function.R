library(here)
# source(here("Parameters_metabolic.R"))
source(here("Functions/Support functions/Pup_growth_function.R"))


pup_growth_cost <- function(m_t, m_tm1){ 

  pup_mass_added <- pup_growth_function(t) # pup age in days since birth

  lipid_energy_pup <- pup_mass_added * prop_lipid_pup(t) * lipid_conversion
  protein_energy_pup <- pup_mass_added * prop_protein_pup(t) * protein_conversion
  
  pup_energy = lipid_energy_pup + protein_energy_pup
  pup_energy  # MJ/d
}

# pup_growth_cost(300)