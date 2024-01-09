if(state == 1){ # S1 ----
    # Non-reproductive female can:
    # die (-2)
    # remain non-reproductive (1)
    # become pregnant (2)
    
    if(newstate == -2){
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = -2            # Decision: Background mortality
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
    } else if(newstate == -1){
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = -1            # Decision: Background mortality
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
    } else if(newstate == 1){
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = NA            # Decision: Continue
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 2){
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 1            # New fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 1             # Decision: Implant
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    }
} else if(state == 2){ # S2 ----
    # Pregnant female can:
    # die (-2)
    # remain pregnant (2)
    # abort (1)
    # give birth (3)
    
    if(newstate == -2){
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = fAge         # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = -3            # Decision: Mortality & Abort
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == -1){
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = fAge         # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = -14           # Decision: Mortality & Abort
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 2){ # Remain pregnant
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = fAge + 1     # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = NA            # Decision: Continue
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 1){ # Abort
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 1            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 2             # Decision: Abort
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 3){ # Give birth
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = sample(pup_masses, size = 1, replace = TRUE) # New pup mass
        Ind[[id]][7,t+1] = 1            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 3             # Decision: Birth
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } 
} else if(state == 3){ # S3 ----
    # Female nursing a pup can:
    # die (-2)
    # continue nursing her pup (3)
    # become pregnant (5)
    # continue nursing pup as juvenile (4)
    # wean/abandon pup (1)
    
    if(newstate == -2){
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = newpmass     # pup mass
        Ind[[id]][7,t+1] = pAge         # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = ifelse(pAge < 243, -4, -5) # Decision: Mortality & Abandon/Wean pup
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        if(pAge >= 243){ # Wean pup; need to be at least 9 months old to wean
            # Assign offspring sex to existing pup
            sex = sample(1:2, 1) # Female = 1; Male = 2
            
            recruit = runif(1)
            
            if(recruit >= (1-recruitment(newpmass, pAge))){ # Recruited
                Ind[[id]][10,t+1] = offspring + 1
                Ind[[id]][11,t] = -6   # Decision: Mortality & Recruit pup
                
                # Female offspring can recruit
                if(sex == 1){ 
                    # Add new individual to population:
                    N_inds = N_inds + 1
                    
                    Ind[[N_inds]][1,t+1] = pAge              # age at recruitment
                    Ind[[N_inds]][2,t+1] = fatMassConverted(newpmass, pAge +1) # mass
                    Ind[[N_inds]][3,t+1] = 1                 # state
                    Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                    Ind[[N_inds]][5,t+1] = 0                 # fetal age
                    Ind[[N_inds]][6,t+1] = 0                 # pup mass
                    Ind[[N_inds]][7,t+1] = 0                 # pup age
                    Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                    Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                    Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                    
                } 
            }
        }
    } else if(newstate == -1){
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = newpmass     # pup mass
        Ind[[id]][7,t+1] = pAge         # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = ifelse(pAge < 243, -15, -16) # Decision: Mortality & Abandon/Wean pup
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        if(pAge >= 243){ # Wean pup; need to be at least 9 months old to wean
            # Assign offspring sex to existing pup
            sex = sample(1:2, 1) # Female = 1; Male = 2
            
            recruit = runif(1)
            
            if(recruit >= (1-recruitment(newpmass, pAge))){ # Recruited
                Ind[[id]][10,t+1] = offspring + 1
                Ind[[id]][11,t] = -17   # Decision: Mortality & Recruit pup
                
                # Female offspring can recruit
                if(sex == 1){ 
                    # Add new individual to population:
                    N_inds = N_inds + 1
                    
                    Ind[[N_inds]][1,t+1] = pAge              # age at recruitment
                    Ind[[N_inds]][2,t+1] = fatMassConverted(newpmass, pAge +1) # mass
                    Ind[[N_inds]][3,t+1] = 1                 # state
                    Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                    Ind[[N_inds]][5,t+1] = 0                 # fetal age
                    Ind[[N_inds]][6,t+1] = 0                 # pup mass
                    Ind[[N_inds]][7,t+1] = 0                 # pup age
                    Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                    Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                    Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                    
                } 
            }
        }
    } else if(newstate == 3){ # Continue nursing pup
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = newpmass     # pup mass
        Ind[[id]][7,t+1] = pAge + 1     # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = NA            # Decision: Continue
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 5){ # Implant
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 1            # fetal age
        Ind[[id]][6,t+1] = newpmass     # pup mass
        Ind[[id]][7,t+1] = pAge + 1     # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 1             # Decision: Implant
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 4){ # Nurse as juvenile
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newpmass     # Juvenile mass
        Ind[[id]][9,t+1] = pAge + 1     # Juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 7             # Decision: Nurse as juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 1){ # Wean or abandon pup
        if(pAge < 243){ # Abandon pup
            
            Ind[[id]][1,t+1] = age + 1      # age
            Ind[[id]][2,t+1] = newmass      # mass
            Ind[[id]][3,t+1] = newstate     # state
            Ind[[id]][4,t+1] = newloc       # location
            Ind[[id]][5,t+1] = 0            # fetal age
            Ind[[id]][6,t+1] = newpmass     # pup mass
            Ind[[id]][7,t+1] = pAge         # pup age
            Ind[[id]][8,t+1] = 0            # juvenile mass
            Ind[[id]][9,t+1] = 0            # juvenile age
            Ind[[id]][10,t+1] = offspring   # offspring recruited
            Ind[[id]][11,t] = 4             # Decision: Abandon pup
            Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
            Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
            
            
        } else if(pAge >= 243){ # Wean pup; need to be at least 9 months old to wean
            
            Ind[[id]][1,t+1] = age + 1      # age
            Ind[[id]][2,t+1] = newmass      # mass
            Ind[[id]][3,t+1] = newstate     # state
            Ind[[id]][4,t+1] = newloc       # location
            Ind[[id]][5,t+1] = 0            # fetal age
            Ind[[id]][6,t+1] = newpmass     # pup mass
            Ind[[id]][7,t+1] = pAge         # pup age
            Ind[[id]][8,t+1] = 0            # juvenile mass
            Ind[[id]][9,t+1] = 0            # juvenile age
            Ind[[id]][10,t+1] = offspring   # offspring recruited
            Ind[[id]][11,t] = 5             # Decision: Wean pup
            Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
            Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
            
            
            # Assign offspring sex to existing pup
            sex = sample(1:2, 1) # Female = 1; Male = 2
            
            recruit = runif(1)
            
            if(recruit >= (1-recruitment(newpmass, pAge))){ # Recruited
                Ind[[id]][10,t+1] = offspring + 1
                Ind[[id]][11,t] = 6   # Decision: Recruit pup
                
                # Female offspring can recruit
                if(sex == 1){ 
                    # Add new individual to population:
                    N_inds = N_inds + 1
                    
                    Ind[[N_inds]][1,t+1] = pAge              # age at recruitment
                    Ind[[N_inds]][2,t+1] = fatMassConverted(newpmass, pAge +1) # mass
                    Ind[[N_inds]][3,t+1] = 1                 # state
                    Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                    Ind[[N_inds]][5,t+1] = 0                 # fetal age
                    Ind[[N_inds]][6,t+1] = 0                 # pup mass
                    Ind[[N_inds]][7,t+1] = 0                 # pup age
                    Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                    Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                    Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                    
                } 
            }
        }
    } 
} else if(state == 4){ # S4 ----
    # Female nursing a juvenile can:
    # die (-2)
    # continue nursing her juvenile (4)
    # become pregnant (6)
    # wean her juvenile (1)
    
    if(newstate == -2){  
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newjmass     # juvenile mass
        Ind[[id]][9,t+1] = jAge         # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = -7            # Decision: Mortality & Wean juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        # Assign offspring sex to existing pup
        sex = sample(1:2, 1) # Female = 1; Male = 2
        
        recruit = runif(1)
        
        if(recruit >= (1-recruitment(newjmass, jAge))){ # Recruited
            Ind[[id]][10,t+1] = offspring + 1
            Ind[[id]][11,t] = -8  # Decision: Mortality & Recruit juvenile
            
            # Female offspring can recruit
            if(sex == 1){ 
                # Add new individual to population:
                N_inds = N_inds + 1
                
                Ind[[N_inds]][1,t+1] = jAge              # age at recruitment
                Ind[[N_inds]][2,t+1] = fatMassConverted(newjmass, jAge +1) # mass
                Ind[[N_inds]][3,t+1] = 1                 # state
                Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                Ind[[N_inds]][5,t+1] = 0                 # fetal age
                Ind[[N_inds]][6,t+1] = 0                 # pup mass
                Ind[[N_inds]][7,t+1] = 0                 # pup age
                Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                
            } 
        }
        
    } else if(newstate == -1){  
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newjmass     # juvenile mass
        Ind[[id]][9,t+1] = jAge         # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = -18           # Decision: Mortality & Wean juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        # Assign offspring sex to existing pup
        sex = sample(1:2, 1) # Female = 1; Male = 2
        
        recruit = runif(1)
        
        if(recruit >= (1-recruitment(newjmass, jAge))){ # Recruited
            Ind[[id]][10,t+1] = offspring + 1
            Ind[[id]][11,t] = -19  # Decision: Mortality & Recruit juvenile
            
            # Female offspring can recruit
            if(sex == 1){ 
                # Add new individual to population:
                N_inds = N_inds + 1
                
                Ind[[N_inds]][1,t+1] = jAge              # age at recruitment
                Ind[[N_inds]][2,t+1] = fatMassConverted(newjmass, jAge +1) # mass
                Ind[[N_inds]][3,t+1] = 1                 # state
                Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                Ind[[N_inds]][5,t+1] = 0                 # fetal age
                Ind[[N_inds]][6,t+1] = 0                 # pup mass
                Ind[[N_inds]][7,t+1] = 0                 # pup age
                Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                
            } 
        }
        
    } else if(newstate == 4){ # Continue nursing juvenile
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newjmass     # juvenile mass
        Ind[[id]][9,t+1] = jAge + 1     # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = NA            # Decision: Continue
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 6){ # Implant
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 1            # New fetal age
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newjmass     # juvenile mass
        Ind[[id]][9,t+1] = jAge + 1     # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 1             # Decision: Implant
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 1){ # Wean juvenile
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 8             # Decision: Wean juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        # Assign offspring sex to existing pup
        sex = sample(1:2, 1) # Female = 1; Male = 2
        
        recruit = runif(1)
        
        if(recruit >= (1-recruitment(newjmass, jAge))){ # Recruited
            Ind[[id]][10,t+1] = offspring + 1
            Ind[[id]][11,t] = 9  # Decision: Recruit juvenile
            
            # Female offspring can recruit
            if(sex == 1){ 
                # Add new individual to population:
                N_inds = N_inds + 1
                
                Ind[[N_inds]][1,t+1] = jAge              # age at recruitment
                Ind[[N_inds]][2,t+1] = fatMassConverted(newjmass, jAge +1) # mass
                Ind[[N_inds]][3,t+1] = 1                 # state
                Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                Ind[[N_inds]][5,t+1] = 0                 # fetal age
                Ind[[N_inds]][6,t+1] = 0                 # pup mass
                Ind[[N_inds]][7,t+1] = 0                 # pup age
                Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                
            } 
        }
    } 
} else if(state == 5){ # S5 ----
    # Pregnant female nursing a pup can:
    # die (-2), 
    # remain pregnant & lactating (5)
    # abort (3)
    # give birth (3)
    # wean/abdanon pup (2)
    # abort & wean/abandon (1)
    # abort & continue nursing pup as juvenile (4)
    
    if(newstate == -2){ 
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = fAge         # fetal age
        Ind[[id]][6,t+1] = newpmass     # pup mass
        Ind[[id]][7,t+1] = pAge         # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited 
        Ind[[id]][11,t] = ifelse(pAge < 243, -12, -13) # Decision: Mortality Abort & Abandon/Wean pup
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        if(pAge >= 243){
            # Assign offspring sex to existing pup
            sex = sample(1:2, 1) # Female = 1; Male = 2
            
            recruit = runif(1)
            
            if(recruit >= (1-recruitment(newpmass, pAge))){ # Recruited
                Ind[[id]][10,t+1] = offspring + 1
                Ind[[id]][11,t] = -14 # Mortality Abort & Recruit pup
                
                # Female offspring can recruit
                if(sex == 1){ 
                    # Add new individual to population:
                    N_inds = N_inds + 1
                    
                    Ind[[N_inds]][1,t+1] = pAge              # age at recruitment
                    Ind[[N_inds]][2,t+1] = fatMassConverted(newpmass, pAge +1) # mass
                    Ind[[N_inds]][3,t+1] = 1                 # state
                    Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                    Ind[[N_inds]][5,t+1] = 0                 # fetal age
                    Ind[[N_inds]][6,t+1] = 0                 # pup mass
                    Ind[[N_inds]][7,t+1] = 0                 # pup age
                    Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                    Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                    Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                    
                } 
            }
        }
        
    } else if(newstate == -1){ 
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = fAge         # fetal age
        Ind[[id]][6,t+1] = newpmass     # pup mass
        Ind[[id]][7,t+1] = pAge         # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited 
        Ind[[id]][11,t] = ifelse(pAge < 243, -20, -21) # Decision: Mortality Abort & Abandon/Wean pup
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        if(pAge >= 243){
            # Assign offspring sex to existing pup
            sex = sample(1:2, 1) # Female = 1; Male = 2
            
            recruit = runif(1)
            
            if(recruit >= (1-recruitment(newpmass, pAge))){ # Recruited
                Ind[[id]][10,t+1] = offspring + 1
                Ind[[id]][11,t] = -22 # Mortality Abort & Recruit pup
                
                # Female offspring can recruit
                if(sex == 1){ 
                    # Add new individual to population:
                    N_inds = N_inds + 1
                    
                    Ind[[N_inds]][1,t+1] = pAge              # age at recruitment
                    Ind[[N_inds]][2,t+1] = fatMassConverted(newpmass, pAge +1) # mass
                    Ind[[N_inds]][3,t+1] = 1                 # state
                    Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                    Ind[[N_inds]][5,t+1] = 0                 # fetal age
                    Ind[[N_inds]][6,t+1] = 0                 # pup mass
                    Ind[[N_inds]][7,t+1] = 0                 # pup age
                    Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                    Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                    Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                    
                } 
            }
        }
        
    } else if(newstate == 5){ # Remain pregnant & lactating
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = fAge + 1     # fetal age
        Ind[[id]][6,t+1] = newpmass     # pup mass
        Ind[[id]][7,t+1] = pAge + 1     # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = NA            # Decision: Continue
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 3){ # Abort or give birth
        if(fAge < 244){ # Abort
            
            Ind[[id]][1,t+1] = age + 1      # age
            Ind[[id]][2,t+1] = newmass      # mass
            Ind[[id]][3,t+1] = newstate     # state
            Ind[[id]][4,t+1] = newloc       # location
            Ind[[id]][5,t+1] = 0            # fetal age
            Ind[[id]][6,t+1] = newpmass     # pup mass
            Ind[[id]][7,t+1] = pAge + 1     # pup age
            Ind[[id]][8,t+1] = 0            # juvenile mass
            Ind[[id]][9,t+1] = 0            # juvenile age
            Ind[[id]][10,t+1] = offspring   # offspring recruited
            Ind[[id]][11,t] = 2             # Decision: Abort
            Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
            Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
            
            
        } else if(fAge >= 244){ # Wean and give birth
            
            Ind[[id]][1,t+1] = age + 1      # age
            Ind[[id]][2,t+1] = newmass      # mass
            Ind[[id]][3,t+1] = newstate     # state
            Ind[[id]][4,t+1] = newloc       # location
            Ind[[id]][5,t+1] = 0            # fetal age
            Ind[[id]][6,t+1] = sample(pup_masses, size = 1, replace = TRUE) # New pup mass
            Ind[[id]][7,t+1] = 1            # New pup age
            Ind[[id]][8,t+1] = 0            # juvenile mass
            Ind[[id]][9,t+1] = 0            # juvenile age
            Ind[[id]][10,t+1] = offspring   # offspring recruited
            Ind[[id]][11,t] = 10            # Decision: Birth & wean pup
            Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
            Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
            
            
            # Assign offspring sex to existing pup
            sex = sample(1:2, 1) # Female = 1; Male = 2
            
            recruit = runif(1)
            
            if(recruit >= (1-recruitment(newpmass, pAge))){ # Recruited
                Ind[[id]][10,t+1] = offspring + 1
                Ind[[id]][11,t] = 11 # Decision: Birth & recruit pup
                
                # Female offspring can recruit
                if(sex == 1){ 
                    # Add new individual to population:
                    N_inds = N_inds + 1
                    
                    Ind[[N_inds]][1,t+1] = pAge              # age at recruitment
                    Ind[[N_inds]][2,t+1] = fatMassConverted(newpmass, pAge +1) # mass
                    Ind[[N_inds]][3,t+1] = 1                 # state
                    Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                    Ind[[N_inds]][5,t+1] = 0                 # fetal age
                    Ind[[N_inds]][6,t+1] = 0                 # pup mass
                    Ind[[N_inds]][7,t+1] = 0                 # pup age
                    Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                    Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                    Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                    
                } 
            }
            
        } 
        
    } else if(newstate == 2){ # Wean or abandon pup
        if(pAge < 243){ # Abandon pup & remain pregnant
            
            Ind[[id]][1,t+1] = age + 1      # age
            Ind[[id]][2,t+1] = newmass      # mass
            Ind[[id]][3,t+1] = newstate     # state
            Ind[[id]][4,t+1] = newloc       # location
            Ind[[id]][5,t+1] = fAge + 1     # fetal age
            Ind[[id]][6,t+1] = 0            # pup mass
            Ind[[id]][7,t+1] = 0            # pup age
            Ind[[id]][8,t+1] = 0            # juvenile mass
            Ind[[id]][9,t+1] = 0            # juvenile age
            Ind[[id]][10,t+1] = offspring   # offspring recruited
            Ind[[id]][11,t] = 4             # Decision: Abandon pup
            Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
            Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
            
            
        } else if(pAge >= 243){ # Wean pup & remain pregnant #fix----
            
            Ind[[id]][1,t+1] = age + 1      # age
            Ind[[id]][2,t+1] = newmass      # mass
            Ind[[id]][3,t+1] = newstate     # state
            Ind[[id]][4,t+1] = newloc       # location
            Ind[[id]][5,t+1] = fAge + 1     # fetal age
            Ind[[id]][6,t+1] = 0            # pup mass
            Ind[[id]][7,t+1] = 0            # pup age
            Ind[[id]][8,t+1] = 0            # juvenile mass
            Ind[[id]][9,t+1] = 0            # juvenile age
            Ind[[id]][10,t+1] = offspring   # offspring recruited
            Ind[[id]][11,t] = 5             # Decision: Wean pup
            Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
            Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
            
            
            # Assign offspring sex to existing pup
            sex = sample(1:2, 1) # Female = 1; Male = 2
            
            recruit = runif(1)
            
            if(recruit >= (1-recruitment(newpmass, pAge))){ # Recruited
                Ind[[id]][10,t+1] = offspring + 1
                Ind[[id]][11,t] = 6 # Decision: Recruit pup
                
                # Female offspring can recruit
                if(sex == 1){ 
                    # Add new individual to population:
                    N_inds = N_inds + 1
                    
                    Ind[[N_inds]][1,t+1] = pAge              # age at recruitment
                    Ind[[N_inds]][2,t+1] = fatMassConverted(newpmass, pAge +1) # mass
                    Ind[[N_inds]][3,t+1] = 1                 # state
                    Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                    Ind[[N_inds]][5,t+1] = 0                 # fetal age
                    Ind[[N_inds]][6,t+1] = 0                 # pup mass
                    Ind[[N_inds]][7,t+1] = 0                 # pup age
                    Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                    Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                    Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                    
                } 
            }
        }
        
    } else if(newstate == 1){ # Abort & wean/abandon pup
        if(pAge < 243){ # Abort & abandon pup
            
            Ind[[id]][1,t+1] = age + 1      # age
            Ind[[id]][2,t+1] = newmass      # mass
            Ind[[id]][3,t+1] = newstate     # state
            Ind[[id]][4,t+1] = newloc       # location
            Ind[[id]][5,t+1] = 0            # fetal age
            Ind[[id]][6,t+1] = 0            # pup mass
            Ind[[id]][7,t+1] = 0            # pup age
            Ind[[id]][8,t+1] = 0            # juvenile mass
            Ind[[id]][9,t+1] = 0            # juvenile age
            Ind[[id]][10,t+1] = offspring   # offspring recruited
            Ind[[id]][11,t] = 12            # Decision: Abort & Abandon pup
            Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
            Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
            
            
        } else if(pAge >= 243){ # Abort & wean pup
            
            Ind[[id]][1,t+1] = age + 1      # age
            Ind[[id]][2,t+1] = newmass      # mass
            Ind[[id]][3,t+1] = newstate     # state
            Ind[[id]][4,t+1] = newloc       # location
            Ind[[id]][5,t+1] = 0            # fetal age
            Ind[[id]][6,t+1] = 0            # pup mass
            Ind[[id]][7,t+1] = 0            # pup age
            Ind[[id]][8,t+1] = 0            # juvenile mass
            Ind[[id]][9,t+1] = 0            # juvenile age
            Ind[[id]][10,t+1] = offspring   # offspring recruited
            Ind[[id]][11,t] = 13            # Decision: Abort & Wean pup
            Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
            Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
            
            
            # Assign offspring sex to existing pup
            sex = sample(1:2, 1) # Female = 1; Male = 2
            
            recruit = runif(1)
            
            if(recruit >= (1-recruitment(newpmass, pAge))){ # Recruited
                Ind[[id]][10,t+1] = offspring + 1
                Ind[[id]][11,t] = 14 # Decision: Abort & recruit pup
                
                # Female offspring can recruit
                if(sex == 1){ 
                    # Add new individual to population:
                    N_inds = N_inds + 1
                    
                    Ind[[N_inds]][1,t+1] = pAge              # age at recruitment
                    Ind[[N_inds]][2,t+1] = fatMassConverted(newpmass, pAge +1) # mass
                    Ind[[N_inds]][3,t+1] = 1                 # state
                    Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                    Ind[[N_inds]][5,t+1] = 0                 # fetal age
                    Ind[[N_inds]][6,t+1] = 0                 # pup mass
                    Ind[[N_inds]][7,t+1] = 0                 # pup age
                    Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                    Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                    Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                    
                } 
            }
        }
        
    } else if(newstate == 4){ # Abort & continue nursing pup as juvenile
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newpmass     # juvenile mass
        Ind[[id]][9,t+1] = pAge + 1     # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 15            # Decision: Abort & nurse as juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    }
} else if(state == 6){ # S6 ----
    # Pregnant female nursing a juvenile can:
    # die (-2), 
    # remain pregnant & lactating (6)
    # abort (4)
    # give birth (3)
    # wean juvenile (2)
    # abort & wean (1)
    
    if(newstate == -2){ 
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = fAge         # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newjmass     # juvenile mass
        Ind[[id]][9,t+1] = jAge         # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = -12            # Decision: Mortality Abort & Wean juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        # Assign offspring sex to existing pup
        sex = sample(1:2, 1) # Female = 1; Male = 2
        
        recruit = runif(1)
        
        if(recruit >= (1-recruitment(newjmass, jAge))){ # Recruited
            Ind[[id]][10,t+1] = offspring + 1
            Ind[[id]][11,t] = -13  # Decision: Mortality Abort & Recruit juvenile
            
            # Female offspring can recruit
            if(sex == 1){ 
                # Add new individual to population:
                N_inds = N_inds + 1
                
                Ind[[N_inds]][1,t+1] = jAge              # age at recruitment
                Ind[[N_inds]][2,t+1] = fatMassConverted(newjmass, jAge +1) # mass
                Ind[[N_inds]][3,t+1] = 1                 # state
                Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                Ind[[N_inds]][5,t+1] = 0                 # fetal age
                Ind[[N_inds]][6,t+1] = 0                 # pup mass
                Ind[[N_inds]][7,t+1] = 0                 # pup age
                Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                
            } 
        }
        
    } else if(newstate == -1){ 
        
        Ind[[id]][1,t+1] = age          # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newstate     # location
        Ind[[id]][5,t+1] = fAge         # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newjmass     # juvenile mass
        Ind[[id]][9,t+1] = jAge         # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = -23            # Decision: Starvation Abort & Wean juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        # Assign offspring sex to existing pup
        sex = sample(1:2, 1) # Female = 1; Male = 2
        
        recruit = runif(1)
        
        if(recruit >= (1-recruitment(newjmass, jAge))){ # Recruited
            Ind[[id]][10,t+1] = offspring + 1
            Ind[[id]][11,t] = -24  # Decision: Starvation Abort & Recruit juvenile
            
            # Female offspring can recruit
            if(sex == 1){ 
                # Add new individual to population:
                N_inds = N_inds + 1
                
                Ind[[N_inds]][1,t+1] = jAge              # age at recruitment
                Ind[[N_inds]][2,t+1] = fatMassConverted(newjmass, jAge +1) # mass
                Ind[[N_inds]][3,t+1] = 1                 # state
                Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                Ind[[N_inds]][5,t+1] = 0                 # fetal age
                Ind[[N_inds]][6,t+1] = 0                 # pup mass
                Ind[[N_inds]][7,t+1] = 0                 # pup age
                Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                
            } 
        }
        
    } else if(newstate == 6){ # Remain pregnant & lactating
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = fAge + 1     # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newjmass     # juvenile mass
        Ind[[id]][9,t+1] = jAge + 1     # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = NA            # Decision: Continue
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 4){ # Abort
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = newjmass     # juvenile mass
        Ind[[id]][9,t+1] = jAge + 1     # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 2             # Decision: Abort
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
    } else if(newstate == 3){ # Give birth & wean juvenile
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = sample(pup_masses, size = 1, replace = TRUE) # New pup mass
        Ind[[id]][7,t+1] = 1            # New pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 16            # Decision: Birth & wean juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        # Assign offspring sex to existing pup
        sex = sample(1:2, 1) # Female = 1; Male = 2
        
        recruit = runif(1)
        
        if(recruit >= (1-recruitment(newjmass, jAge))){ # Recruited
            Ind[[id]][10,t+1] = offspring + 1
            Ind[[id]][11,t] = 17 # Decision: Birth & recruit juvenile
            
            # Female offspring can recruit
            if(sex == 1){ 
                # Add new individual to population:
                N_inds = N_inds + 1
                
                Ind[[N_inds]][1,t+1] = jAge              # age at recruitment
                Ind[[N_inds]][2,t+1] = fatMassConverted(newjmass, jAge +1) # mass
                Ind[[N_inds]][3,t+1] = 1                 # state
                Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                Ind[[N_inds]][5,t+1] = 0                 # fetal age
                Ind[[N_inds]][6,t+1] = 0                 # pup mass
                Ind[[N_inds]][7,t+1] = 0                 # pup age
                Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                
            } 
        }
        
    } else if(newstate == 2){ # Wean juvenile
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = fAge + 1     # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 8             # Decision: Wean juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        # Assign offspring sex to existing pup
        sex = sample(1:2, 1) # Female = 1; Male = 2
        
        recruit = runif(1)
        
        if(recruit >= (1-recruitment(newjmass, jAge))){ # Recruited
            Ind[[id]][10,t+1] = offspring + 1
            Ind[[id]][11,t] = 9  # Decision: Recruit juvenile
            
            # Female offspring can recruit
            if(sex == 1){ 
                # Add new individual to population:
                N_inds = N_inds + 1
                
                Ind[[N_inds]][1,t+1] = jAge              # age at recruitment
                Ind[[N_inds]][2,t+1] = fatMassConverted(newjmass, jAge +1) # mass
                Ind[[N_inds]][3,t+1] = 1                 # state
                Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                Ind[[N_inds]][5,t+1] = 0                 # fetal age
                Ind[[N_inds]][6,t+1] = 0                 # pup mass
                Ind[[N_inds]][7,t+1] = 0                 # pup age
                Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                
            } 
        }
        
    } else if(newstate == 1){ # Abort & wean juvenile
        
        Ind[[id]][1,t+1] = age + 1      # age
        Ind[[id]][2,t+1] = newmass      # mass
        Ind[[id]][3,t+1] = newstate     # state
        Ind[[id]][4,t+1] = newloc       # location
        Ind[[id]][5,t+1] = 0            # fetal age
        Ind[[id]][6,t+1] = 0            # pup mass
        Ind[[id]][7,t+1] = 0            # pup age
        Ind[[id]][8,t+1] = 0            # juvenile mass
        Ind[[id]][9,t+1] = 0            # juvenile age
        Ind[[id]][10,t+1] = offspring   # offspring recruited
        Ind[[id]][11,t] = 18            # Decision: Abort & wean juvenile
        Ind[[id]][12,t+1] = metabolic_scalar   # Metabolic scalar
        Ind[[id]][13,t+1] = ifelse(newloc == 2, foraging_days+1, 0)  # Foraging days
        
        
        # Assign offspring sex to existing pup
        sex = sample(1:2, 1) # Female = 1; Male = 2
        
        recruit = runif(1)
        
        if(recruit >= (1-recruitment(newjmass, jAge))){ # Recruited
            Ind[[id]][10,t+1] = offspring + 1
            Ind[[id]][11,t] = 19 # Decision: Abort & recruit juvenile
            
            # Female offspring can recruit
            if(sex == 1){ 
                # Add new individual to population:
                N_inds = N_inds + 1
                
                Ind[[N_inds]][1,t+1] = jAge              # age at recruitment
                Ind[[N_inds]][2,t+1] = fatMassConverted(newjmass, jAge +1) # mass
                Ind[[N_inds]][3,t+1] = 1                 # state
                Ind[[N_inds]][4,t+1] = sample(1:2, 1)    # location randomly assigned
                Ind[[N_inds]][5,t+1] = 0                 # fetal age
                Ind[[N_inds]][6,t+1] = 0                 # pup mass
                Ind[[N_inds]][7,t+1] = 0                 # pup age
                Ind[[N_inds]][8,t+1] = 0                 # juvenile mass
                Ind[[N_inds]][9,t+1] = 0                 # juvenile age
                Ind[[N_inds]][10,t+1] = 0                # cumulative offspring
                
            } 
        }
        
    }
}