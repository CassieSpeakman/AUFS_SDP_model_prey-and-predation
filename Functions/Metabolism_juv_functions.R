# FMR_land and FMR_sea for juvenile AUFS
# Author: CN Speakman

# Input (m) is the mass of the female
# Using scaled female metabolic rates

FMR_J_sea <- function(m){
  modifier = 1.4
  
  modified_rate = 0.96*modifier
  
  csl_fmr_j = modified_rate*m^0.75
  csl_fmr_j
}

FMR_J_land <- function(m){
  modifier = 1.4
  
  modified_rate = 0.6*modifier
  
  csl_fmr_j = modified_rate*m^0.75
  csl_fmr_j
}