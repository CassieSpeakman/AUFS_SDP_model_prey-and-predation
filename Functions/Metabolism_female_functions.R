# FMR_land, FMR_sea & BMR for adult AUFS
# Author: CN Speakman

# Input (m) is the mass of the female
# S = reproductive state, separating non-lactating (S1-2) and lactating (S3-6)
# Metabolic rates based on California sea lions (CSL)

FMR_sea <- function(m, s = 3){
    if(s == 1 | s == 2){
        csl_fmr_s = 0.96*m^0.75
        csl_fmr_s  
    } else if (s >= 3) {
        csl_fmr_s = 1.38*m^0.75
        csl_fmr_s
    }
}

FMR_land <- function(m){ 
  
  csl_fmr_l = 0.6*m^0.75
  csl_fmr_l
}
