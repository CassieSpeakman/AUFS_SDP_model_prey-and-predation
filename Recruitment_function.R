# Probability of recruitment as a function of mass at weaning
# Author: CN Speakman
# Updated: August 2023

# Input (mass) is the mass of the pup or juvenile at time t

recruitment <- function(mass, age){
    y = 7 # Scaling exponent for mass
    x50 = 15 # Mass at which recruitment probability is 50%
    recruit = (mass^y/(mass^y+x50^y))*0.8
    
    z = 20 # Scaling exponent for age
    w50 = 250 # Age at which recruitment probability is 50% of a 1 year old pup
    age_scaler = (age^z/(age^z+w50^z))
    
    recruit = recruit*age_scaler
    return(recruit)
}