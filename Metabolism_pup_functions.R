# BMR for AUFS pups
# Author: CN Speakman

# Input (mass) is the mass of the pup in kg
# Input (age) is the age of the pup in days
# 
library(here)
source(here("Functions/Support functions/Metabolic_rate_conversions.R"))

MR_pup <- function(mass, age){
  if(age <= 7){ # Perinatal period
    MR = convert_mlo2_h_mj(ave_mass = 6, mlo2 = 1.74) # values from Table 1 McDonald et al 2012
    periMR = MR*mass^0.75
    return(periMR)

  } else if(age > 7 & age <= 105){ # Premoult period
    MR = convert_mlo2_h_mj(ave_mass = 9.6, mlo2 = 1.76) # values from Table 1 McDonald et al 2012
    preMR = MR*mass^0.75
    return(preMR)

  } else if(age > 105){ # Postmoult period
    MR = convert_mlo2_h_mj(ave_mass = 12.1, mlo2 = 1.77) # values from Table 1 McDonald et al 2012
    postMR = MR*mass^0.75
    return(postMR)

  }
}
