# Juvenile foraging 
# This function adjusts the prey acquisition of females to a learning ability
# of juveniles. I.e., younger juveniles will be less efficient foragers and 
# will need to learn to forage effectively. 

# This calculation produces a sigmoidal curve to ensure a period of learning before 
# the juvenile asymptotes at 99.9% of adult foraging competence at age 2. 

# Input (juv_age) is the juvenile age in days

a_rel <- function(juv_age){
    y = 4 # scaling constant
    x50 = 274 # age in days that the juvenile foraging ability is assumed to reach 50% of the adult female
    
    ability = (juv_age^y/(juv_age^y+x50^y))
    
    return(ability)
}