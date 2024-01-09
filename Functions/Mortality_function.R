# Age-specific mortality rates for female Australian fur seals
# Author: CN Speakman
# Updated: August 2023

# Based on survival data from Gibbens & Arnould 2009

mortalitySea <- function(scenario, age){
    if (scenario == "base"){
        mortality <- (-4.14132E-15*(age)^3 + 1.44095E-10*(age)^2 - 5.86349E-7*(age) + 8.63359E-4)
        return(mortality)
    } else if (scenario == "ten"){
        mortality <- (-4.14132E-15*(age)^3 + 1.44095E-10*(age)^2 - 5.86349E-7*(age) + 1.00035E-3)
        return(mortality)
    } else if (scenario == "twenty"){
        mortality <- (-4.14132E-15*(age)^3 + 1.44095E-10*(age)^2 - 5.86349E-7*(age) + 1.27432E-3)
        return(mortality)
    } 
}




mortalityLand <- function(scenario, age){
      mortality <- (-4.14132E-15*(age)^3 + 1.44095E-10*(age)^2 - 5.86349E-7*(age) + 7.26373E-4)
      return(mortality)
}
