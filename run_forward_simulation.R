# Forward simulation source file for AUFS SDP model
# Author: CN Speakman
# Date created: 15/08/2022
# Date updated: 23/11/2023

# This file runs the forward simulation

# Create overarching holding lists:
Ind <- list()

# Generate population -------------------------------------------------------------
Ind <- set_population(N_inds = N_inds, Max_inds = Max_inds, endT = endT)

for(t in startT:endT){ # Cycle through days
    
    if (t == endT){ 
        # Break the script when t == endT
        # Script would look for t+1 and break because it doesn't exist - this works around the problem
        break
    }
    
    for(id in 1:Max_inds){ # Save details ----
        # for(id in 1:Max_inds){
        
        age = as.numeric(Ind[[id]][1,t])
        mass = as.numeric(Ind[[id]][2,t])
        state = as.numeric(Ind[[id]][3,t])
        loc = as.numeric(Ind[[id]][4,t])
        fAge = as.numeric(Ind[[id]][5,t])
        pMass = as.numeric(Ind[[id]][6,t])
        pAge = as.numeric(Ind[[id]][7,t])
        jMass = as.numeric(Ind[[id]][8,t])
        jAge = as.numeric(Ind[[id]][9,t])
        offspring = as.numeric(Ind[[id]][10,t])
        metabolic_scalar = as.numeric(Ind[[id]][12,t])
        foraging_days = as.numeric(Ind[[id]][13,t])
        
        if(state > 0){ # Alive individuals
            prevloc = ifelse(t == 1, loc, Ind[[id]][4,t-1])
            
            if(prevloc == 12 | prevloc == 0){
                prevloc = sample(1:2,1, prob = c(0.3,0.7))
            }
            
            if(age < yearOne){ # JUVENILE ---------------------------------
                # Can only be non-reproductive as a juvenile
                
                if(state == 1){ # S1: Non-reproductive ----
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, mortality)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, scenario = prey_scenario, firstDay = "Yes", productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))),age,loc]    
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                          R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, mortality)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, scenario = prey_scenario, firstDay = "Yes", productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, prevloc, newstate, mortality)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else {
                    stop("Invalid state for juvenile")
                } # End states for juvenile
                
                #.----
            } else if (age >= yearOne & age < yearTwo){ # FIRST REPRODUCTIVE YEAR -----------------------------------------------------------
                # In the first year of sexual maturity, a female can only be non-reproductive (state 1) or pregnant (state 2).
                
                if(state == 1){ # S1: Non-reproductive -------------------------------------
                    if(loc == 1){ # __Land ----
                        # Calculate new mass- & location:
                        newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, mortality)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]   
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, prevloc, newstate, mortality)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, prevloc, newstate, mortality)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else if(state == 2){ # S2: Pregnant -----------------------------------------------------
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newmass = new_female_mass_land(fm = mass, age = age, fage = fAge, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newmass = new_female_mass_land(fm = mass, age = age, fage = fAge, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else {
                    stop("State invalid for 1st year")
                } # End states for 1st year
                
                #.----
            } else if (age >= yearTwo & age < yearThree){ # SECOND YEAR ----------------------------------------------------------------
                # In the 2nd year of sexual maturity, a female can be non-reproductive (state 1), pregnant (state 2), or nursing a pup (state 3). 
                
                if(state == 1){ # S1: Non-reproductive -------------------------------------
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]   
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else if(state == 2){ # S2: Pregnant -----------------------------------------------------
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newmass = new_female_mass_land(fm = mass, age = age, fage = fAge, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newmass = new_female_mass_land(fm = mass, age = age, fage = fAge, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                    
                } else if(state == 3){ # S3: Nursing pup ---------------------------------------------------
                    
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) # new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                        newmass = new_female_mass_nursing(fm = mass, om = newpmass, oa = pAge, age = age, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        
                        newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                             ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                    R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) # new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                            newmass = new_female_mass_nursing(fm = mass, om = newpmass, oa = pAge, age = age, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else if(state == 5){ # S5: Nursing pup & pregnant ----
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                        newmass = new_female_mass_nursing(fm = mass, om = newpmass, age = age, oa = pAge, fage = fAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        
                        newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){ # Land
                            # Calculate new mass:
                            newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                            newmass = new_female_mass_nursing(fm = mass, om = newpmass, age = age, oa = pAge, fage = fAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){ # Sea
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                    
                } else {
                    stop("State invalid for 2nd year")
                }
                
                #.----
            } else if (age >= yearThree & age < yearSenescence){ # THIRD YEAR ------------------------------------------------------
                # All other reproductive years, a female can be in any of the 6 reproductive states.
                
                if(state == 1){ # S1: Non-reproductive -------------------------------------
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]   
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else if(state == 2){ # S2: Pregnant -----------------------------------------------------
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newmass = new_female_mass_land(fm = mass, age = age, fage = fAge, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newmass = new_female_mass_land(fm = mass, age = age, fage = fAge, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                    
                } else if(state == 3){ # S3: Nursing pup ---------------------------------------------------
                    
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) # new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                        newmass = new_female_mass_nursing(fm = mass, om = newpmass, oa = pAge, age = age, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) # new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                            newmass = new_female_mass_nursing(fm = mass, om = newpmass, oa = pAge, age = age, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else if(state == 4){ # S4: Nursing juvenile ---------------------------------------------------
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newjmass = new_juvenile_mass_nursing(jm = jMass, age = jAge, scenario = prey_scenario) 
                        newmass = new_female_mass_nursing(fm = mass, om = newjmass, age = age, oa = jAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newjmass)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = jAge, om = jMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = jAge, om = jMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newjmass = new_juvenile_mass_fasting(jm = jMass, age = jAge)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newjmass)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newjmass = new_juvenile_mass_nursing(jm = jMass, age = jAge, scenario = prey_scenario) 
                            newmass = new_female_mass_nursing(fm = mass, om = newjmass, age = age, oa = jAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = jAge, om = jMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = jAge, om = jMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            newjmass = new_juvenile_mass_fasting(jm = jMass, age = jAge)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate, newjmass)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                    
                } else if(state == 5){ # S5: Nursing pup & pregnant ----
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                        newmass = new_female_mass_nursing(fm = mass, om = newpmass, age = age, oa = pAge, fage = fAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){ # Land
                            # Calculate new mass:
                            newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                            newmass = new_female_mass_nursing(fm = mass, om = newpmass, age = age, oa = pAge, fage = fAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){ # Sea
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, fage = fAge, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                    
                } else if(state == 6){ # S6: Nursing juvenile & pregnant -------------------
                    
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newjmass = new_juvenile_mass_nursing(jm = jMass, age = jAge, scenario = prey_scenario) 
                        newmass = new_female_mass_nursing(fm = mass, om = newjmass, age = age, oa = jAge, fage = fAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newjmass)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, , age = age, fage = jAge, oa = pAge, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, , age = age, fage = jAge, oa = pAge, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newjmass = new_juvenile_mass_fasting(jm = jMass, age = jAge)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newjmass)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newjmass = new_juvenile_mass_nursing(jm = jMass, age = jAge, scenario = prey_scenario) 
                            newmass = new_female_mass_nursing(fm = mass, om = newjmass, age = age, oa = jAge, fage = fAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, , age = age, fage = jAge, oa = pAge, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, , age = age, fage = jAge, oa = pAge, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            newjmass = new_juvenile_mass_fasting(jm = jMass, age = jAge)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate, newjmass)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                    
                } else {
                    stop("Invalid state")
                } # End state loop # End state loop
                
                #.----
            } else if (age >= yearSenescence & age < 7665){ # SENESCENCE ------------------------------------------------------
                # In the year of reproductive senescence, a female can be non-reproductive (state 1), nursing a pup (state 3) or juvenile (state 4).
                
                if(state == 1){ # S1: Non-reproductive -------------------------------------
                    if(loc == 1){ # __Land ----
                        # Calculate new mass & location:
                        newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]       
                        newloc = ifelse(newloc == 12, sample(1:2, 1, prob = c(0.3,0.7)), newloc)
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass & location:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), age, loc]
                        newloc = ifelse(newloc == 12, sample(1:2, 1, prob = c(0.3,0.7)), newloc)
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newmass = new_female_mass_land(fm = mass, age = age, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = 0, om = 0, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else if(state == 3){ # S3: Nursing pup ---------------------------------------------------
                    
                    if(loc == 1){ # __Land ----
                        # Calculate new mass:
                        newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                        newmass = new_female_mass_nursing(fm = mass, om = newpmass, oa = pAge, age = age, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newpmass = new_pup_mass_nursing(pm = pMass, age = pAge, scenario = prey_scenario) 
                            newmass = new_female_mass_nursing(fm = mass, om = newpmass, oa = pAge, age = age, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = pAge, om = pMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            newpmass = new_pup_mass_fasting(pm = pMass, age = pAge)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newpmass), floor(maximumMass(pAge, class = "offspring"))), age, newloc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate, newpmass)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else if(state == 4){ # S4: Nursing juvenile ---------------------------------------------------
                    # A female can continue nursing her juvenile or may wean in the year of senescence.
                    
                    if(loc == 1){ # __Land ----
                        # Calculate new mass:
                        newjmass = new_juvenile_mass_nursing(jm = jMass, age = jAge, scenario = prey_scenario) 
                        newmass = new_female_mass_nursing(fm = mass, om = newjmass, age = age, oa = jAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newjmass)
                        
                    } else if (loc == 2){ # __Sea ----
                        # Calculate new mass:
                        if(prevloc == 1){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = jAge, om = jMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                        } else if (prevloc == 2){
                            newmass = new_female_mass_foraging(fm = mass, age = age, oa = jAge, om = jMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                        }
                        newjmass = new_juvenile_mass_fasting(jm = jMass, age = jAge)
                        newloc = L[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc]
                        
                        # Update state:
                        mortality = runif(1) # Randomised mortality 
                        newstate = ifelse(newmass < criticalMass(age), -1, 
                                          ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                 R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                        
                        # Save values into arrays
                        source(here("Functions/Assign_values_updated.R"))
                        
                        rm(newmass, newloc, newstate, newjmass)
                        
                    } else if (loc == 12){ # __Either ----
                        # Update location:
                        newloc = sample(1:2, 1, prob = c(0.3,0.7))
                        
                        if(newloc == 1){
                            # Calculate new mass:
                            newjmass = new_juvenile_mass_nursing(jm = jMass, age = jAge, scenario = prey_scenario) 
                            newmass = new_female_mass_nursing(fm = mass, om = newjmass, age = age, oa = jAge, scenario = prey_scenario, metabolic_scalar = metabolic_scalar)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalityLand(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                            
                        } else if (newloc == 2){
                            # Calculate new mass:
                            if(prevloc == 1){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = jAge, om = jMass, scenario = prey_scenario, productive_areas = productive_areas, firstDay = "Yes", metabolic_scalar = metabolic_scalar)
                            } else if (prevloc == 2){
                                newmass = new_female_mass_foraging(fm = mass, age = age, oa = jAge, om = jMass, scenario = prey_scenario, productive_areas = productive_areas, metabolic_scalar = metabolic_scalar)
                            }
                            newjmass = new_juvenile_mass_fasting(jm = jMass, age = jAge)
                            
                            # Update state:
                            mortality = runif(1) # Randomised mortality 
                            newstate = ifelse(newmass < criticalMass(age), -1, 
                                              ifelse(mortality > mortalitySea(scenario = mortality_scenario, age = age), 
                                                     R[[state]][min(round(newmass), floor(maximumMass(age))), min(round(newjmass), floor(maximumMass(jAge, class = "offspring"))), age, loc], -2))
                            
                            # Save values into arrays
                            source(here("Functions/Assign_values_updated.R"))
                        }
                        
                        rm(newmass, newloc, newstate, newjmass)
                        
                    } else {
                        stop("Invalid location")
                    } # End location
                    
                } else {
                    stop("Invalid state for senescence")
                } # End state loop
                
            } # End age loop ---
            
        } # else if(state == -1 | state == -2){ # End alive loop --- 
        
        # }
    } # End individual loop ---
} # End time loop ---

