# Placental growth rate function
# Used to calculate the growth rate over gestation 
# to determine the costs of placenta growth
# for the female

# This is a source function for the Placental_energy_function

# Placental weight calculated using Boyd & McCann 1989 relationship for AFS
# log10 placental mass (kg) = (0.722 * log10 foetus mass (kg)) -0.788
# converted back to kg (i.e. 10^ log10 placental mass)

placental_growth_function <- function(t){
  # ifelse(t < 50, 0, (5*10^-14)*(t-50)^(3.4))
  # ifelse t < 50 used due to placental mass equaling approximately zero in early gestation
  
  # Updated calculation
  0.00000008*t^0.7586
}
