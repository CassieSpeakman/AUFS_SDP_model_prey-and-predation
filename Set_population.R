
set_population <- function(N_inds, Max_inds, endT){
  
  # Generate population -------------------------------------------------------------
  age_prob = c(0.140, 0.111, 0.095, 0.086, 0.079, 0.074, 0.070,
               0.065, 0.060, 0.054, 0.047, 0.039, 0.030, 0.022,
               0.014, 0.008, 0.004, 0.002, 0.000, 0.000) # Probability of age occurrence for age-structured population

  # Populate individual arrays:
  for(i in 1:N_inds){
    Ind[[i]] = array(0, dim = c(14, endT)) # dimnames = list(c("Age", "Mass", "State", "Location", "FoetalAge", "PupMass", "PupAge", "Juvmass", "Juvage", "CumOff", "Scalar")))
    Ind[[i]][1,1] = sample(seq(365, 7300, by=365), size = 1, replace = TRUE, prob = age_prob)    # Assign age based on age structure
    # Ind[[i]][2,1] = sample(rnorm(n = 1000, mean = aveMass(Ind[[i]][1,1])*0.15, sd = 0.5), size = 1, replace = TRUE) # Assign age specific mass
    Ind[[i]][2,1] = aveMass(Ind[[i]][1,1])*0.2 # Assign age specific mass
    
    # Assign states:
    if(Ind[[i]][1,1] < 1460){
      Ind[[i]][3,1] = 1 # Assign state; can only be non-reproductive at t = 1
    # } else if (Ind[[i]][1,1] == 1460){
    #   Ind[[i]][3,1] = sample(c(1,3), size = 1, replace = TRUE, prob = c(0.2,0.8)) # Assign state; can be non-reproductive or nursing pup
    # } else if (Ind[[i]][1,1] > 1460){
    #   Ind[[i]][3,1] = sample(c(1,3,4), size = 1, replace = TRUE, prob = c(0.15,0.8,0.05)) # Assign state; can be non-reproductive or nursing pup/juvenile
    # } # End states
    } else if (Ind[[i]][1,1] >= 1460){
      Ind[[i]][3,1] = sample(c(1,3), size = 1, replace = TRUE, prob = c(0.15,0.85)) # Assign state; can be non-reproductive or nursing pup
    } # End states
    
      
    # Assign pup and juvenile masses:
    if(Ind[[i]][3,1] == 3){
      Ind[[i]][6,1] = sample(pup_masses, size = 1, replace = TRUE)
      Ind[[i]][7,1] = 1
      Ind[[i]][4,1] = 1 # Assign location (1 land & 2 sea)
    } else if (Ind[[i]][3,1] == 4) {
      Ind[[i]][8,1] = sample(rnorm(n = 1000, mean = 19.4, sd = 0.5), size = 1, replace = TRUE)
      Ind[[i]][9,1] = 365
      Ind[[i]][4,1] = sample(c(1,2), size = 1, replace = TRUE) # Assign location (1 land & 2 sea)
    } else {
      Ind[[i]][4,1] = sample(c(1,2), size = 1, replace = TRUE) # Assign location (1 land & 2 sea)
    } # End offspring mass
    
    Ind[[i]][10,1] = 0 # Number of offspring 
    
    Ind[[i]][12,1] = runif(1, 0.90, 1.10) # Metabolic scalar 
  }
  
  for(i in (N_inds+1):Max_inds){
    Ind[[i]] = array(0, dim = c(14, endT)) # dimnames = list(c("Age", "Mass", "State", "Location", "FoetalAge", "PupMass", "PupAge", "Juvmass", "Juvage", "CumOff", "MetabolicScalar", "DietScalar", "SuccessScalar")))
    Ind[[i]][12,1] = runif(1, 0.90, 1.10) # Metabolic scalar 
  }
  
  rm(age_prob)
  return(Ind)
}
