# Foetal growth rate function
# Used to calculate the growth rate over gestation 
# to determine the costs of foetus growth
# for the female

# This is a source function for the Foetal_energy_function


foetal_growth_function <- function(t){
  # ifelse(t < 105, 0, (8*10^-11)*(t-105)^(3.94)) # based on foetal growth data from JPYA
  # ifelse t < 105 used due to foetal mass equaling approximately zero in early gestation
  
  ifelse(t < 70, 0, (0.0004*t-0.0277)) # based on foetal growth data from JPYA
  # Updated to account for shorter gestation and adjusted foetal age
  # ifelse t < 70 used due to foetal mass equaling approximately zero in early gestation
} 
