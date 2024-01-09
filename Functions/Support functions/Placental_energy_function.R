# Placental energy function
# Used to calculate the energetic costs of placenta growth
# for the female based on the amount of new mass added, 
# combined with the energy density of that new mass

library(here)
# source(here("Parameters_metabolic.R"))
source(here("Functions/Support functions/Placental_growth_function.R"))

stored_placental_energy <- function(t){ 
  placental_mass_added <- placental_growth_function(t)
  placental_energy = placental_mass_added*placental_conversion
  return(placental_energy)
}
