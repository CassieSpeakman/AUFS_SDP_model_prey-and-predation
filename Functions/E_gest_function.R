# Functions for determining the daily costs of gestation 
# Author: CN Speakman
# Updated: June 2023

# Includes all functions for calculating the cost of gestation


# Foetal growth rate function ----
# Used to calculate the growth rate over gestation 
# to determine the costs of foetus growth
# for the female

foetal_growth_function <- function(t){
    # ifelse(t < 105, 0, (8*10^-11)*(t-105)^(3.94)) # based on foetal growth data from JPYA
    # ifelse t < 105 used due to foetal mass equaling approximately zero in early gestation
    
    ifelse(t < 70, 0, (0.0004*t-0.0277)) # based on foetal growth data from JPYA
    # Updated to account for shorter gestation and adjusted foetal age
    # ifelse t < 70 used due to foetal mass equaling approximately zero in early gestation
} 




# Foetal energy function ----
# Used to calculate the energetic costs of foetal growth
# for the female based on the amount of new mass added, 
# combined with the energy density of that new mass

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




# Placental growth rate function ----
# Used to calculate the growth rate over gestation 
# to determine the costs of placenta growth
# for the female

# Placental weight calculated using Boyd & McCann 1989 relationship for AFS
# log10 placental mass (kg) = (0.722 * log10 foetus mass (kg)) -0.788
# converted back to kg (i.e. 10^ log10 placental mass)

placental_growth_function <- function(t){
    0.00000008*t^0.7586
}




# Placental energy function ----
# Used to calculate the energetic costs of placenta growth
# for the female based on the amount of new mass added, 
# combined with the energy density of that new mass

stored_placental_energy <- function(t){ 
    placental_mass_added <- placental_growth_function(t)
    placental_energy = placental_mass_added*placental_conversion
    return(placental_energy)
}




HIG <- function(t){
    ifelse(t<75, 0, ((1*10^-7)*t^3)+((4*10^-5)*t^2)-(0.0046*t)+0.0735)
}




# Total daily costs of gestation ----

# Input (t) is the foetal age in days.
# This is based on the gestation days (i.e daily time step)

# This script calculates the HIG and the amount of stored energy in the 
# foetus & placenta at time t, then adds these together for the total 
# cost of gestation.


E_gest <- function(t){
    HIG <- HIG(t) # heat increment of gestation at time t
    stored_foetal_energy <- stored_foetal_energy(t) # stored foetal energy at time t
    stored_placental_energy <- stored_placental_energy(t) # stored placental energy at time t
    
    cost_gestation = HIG + stored_foetal_energy + stored_placental_energy
    return(cost_gestation) # MJ/d
}