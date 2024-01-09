# Converts total mass to fat mass for recruited offspring

fatMassConverted <- function(om, oa){
    y1 = 0.05;  x1 = criticalMass(age = oa, class = "o")
    y2 = 0.20;  x2 = maximumMass(age = oa, class = "o")
    
    m = (y2-y1)/(x2-x1)
    c = y1 - (m*x1)
    
    converted = (m*om + c)*om
    return(converted)
} 