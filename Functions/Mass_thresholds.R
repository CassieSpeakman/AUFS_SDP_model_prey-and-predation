# Mass thresholds
# Updated: August 2023

aveMass <- function(age){
    if (age <= 180){
        mass = (0.0591*age) + 7.8568 # From JPYA pup composition data
        return(mass)

    } else if (age > 180 & age <= 730){
        mass = (0.0324*age) + 12.66 # From JPYA pup composition data
        return(mass)

    } else if(age > 730){
        k = 0.36; a0 = -1.86; As = 85.4
        mass = As*(1-exp(-k*((age/365)-a0)))^3
        # where k is the growth coefficient, a0 is the theoretical age when size is zero, and As is asymptotic size.

        return(mass)
    }
}

criticalMass <- function(age, class = "adult"){
    crit_mass = ifelse(class == "adult",
                       aveMass(age)*0.05,
                       aveMass(age)*0.8)

    return(crit_mass)
}

maximumMass <- function(age, class = "adult"){
    max_mass = ifelse(class == "adult",
                      aveMass(age)*0.2,
                      aveMass(age)*1.2)

    return(max_mass)
}