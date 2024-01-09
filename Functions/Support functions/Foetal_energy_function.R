# Foetal energy function
# Used to calculate the energetic costs of foetal growth
# for the female based on the amount of new mass added, 
# combined with the energy density of that new mass

library(here)
source(here("Functions/Support functions/Foetal_growth_function.R"))

stored_foetal_energy <- function(t){ 
 
  prop_lipid = 0.104 # assuming value at birth for AUFS
  prop_water = 0.664 # assuming value at birth for AUFS
  prop_lean =	0.231 # assuming value at birth for AUFS
  
  foetal_mass_added <- foetal_growth_function(t)
  
  lipid_energy <- foetal_mass_added*prop_lipid*lipid_conversion
  protein_energy <- foetal_mass_added*prop_lean*protein_conversion
  
  foetal_energy = lipid_energy + protein_energy
  
  return(foetal_energy)
}
