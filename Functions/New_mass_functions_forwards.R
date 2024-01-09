# New mass calculation functions for all possible female choices
# Updated: 10/10/2023


# Females ----
new_female_mass_nursing <- function(fm, age, om, oa, fage = 0, scenario = "base", metabolic_scalar = 1){
    # Pup requirements:
    max_pup = maximumMass(age = ifelse(oa <=365, 365, 730), class = "offspring")
    energy_required = ifelse(om < max_pup, (((max_pup - om)*0.54)*lipid_conversion) + (((max_pup - om)*0.46)*protein_conversion), 0)
    
    # Female energy:
    max_fm = maximumMass(age = age)
    metabolism = FMR_land(aveMass(age)) * metabolic_scalar
    growth = ifelse(age <= 4 *365, 0.035, ifelse(age > 4*365 & age <= 6*365, 0.02, ifelse(age > 6*365 & age <= 8*365, 0.011, ifelse(age > 8*365 & age < 10*365, 0.005, 0))))*blubber_conversion
    gestation = ifelse(fage > 0, E_gest(t = fage), 0)
    lactation = ifelse(energy_required < E_lact(om, oa), 0, E_lact(om, oa))
    
    new_mass = min(max(fm - ((metabolism + growth + lactation + gestation)/lipid_conversion), 1), max_fm)
    return(new_mass)
}

new_female_mass_land <- function(fm, age, fage = 0, metabolic_scalar = 1){
    metabolism = ifelse(age < 1095, FMR_J_land(fm), FMR_land(aveMass(age))) * metabolic_scalar
    growth = ifelse(age <= 4 *365, 0.035, ifelse(age > 4*365 & age <= 6*365, 0.02, ifelse(age > 6*365 & age <= 8*365, 0.011, ifelse(age > 8*365 & age < 10*365, 0.005, 0))))*blubber_conversion
    gestation = ifelse(fage > 0, E_gest(t = fage), 0)
    
    new_mass = max(fm - ((metabolism + growth + gestation)/lipid_conversion), 1) 
    return(new_mass)  
}

new_female_mass_foraging <- function(fm, age, oa, om, fage = 0, scenario = "base", firstDay = "No", foraging_attempts, metabolic_scalar = 1){
    attempts = foraging_attempts; successes = sum(ifelse((runif(attempts) < (prob_food)^(1/attempts)) == TRUE, 1, 0))
    energy_intake = ifelse(firstDay == "Yes", 
                           ((net_energy(fm = fm, age = age, oa = oa, om = om, scenario = scenario)*0.75) * successes)/attempts,
                           (net_energy(fm = fm, age = age, oa = oa, om = om, scenario = scenario) * successes)/attempts)
    
    state = ifelse(oa > 0, 3, 1)
    metabolism = ifelse(age < 1095, FMR_J_sea(fm), FMR_sea(aveMass(age), state)) * metabolic_scalar
    growth = ifelse(age <= 4 *365, 0.035, ifelse(age > 4*365 & age <= 6*365, 0.02, ifelse(age > 6*365 & age <= 8*365, 0.011, ifelse(age > 8*365 & age < 10*365, 0.005, 0))))*blubber_conversion
    gestation = ifelse(fage > 0, E_gest(t = fage), 0)
    
    new_mass = min(fm + ((energy_intake - metabolism - growth - gestation)/lipid_conversion), maximumMass(age))
    return(new_mass)
    
}




# Juveniles ----
new_juvenile_mass_foraging <- function(jm, age, scenario = "base", firstDay = "No", foraging_attempts, metabolic_scalar = 1){
    maximum_mass = maximumMass(age = 730, class = "offspring")
    attempts = foraging_attempts; successes = sum(ifelse((runif(attempts) < (prob_food)^(1/attempts)) == TRUE, 1, 0))
    
    energy_intake = ifelse(firstDay == "Yes", 
                           (((a_rel(age)*net_energy(fm = jm, age = age, oa = 0, scenario = scenario))*0.75) * successes)/attempts,
                           ((a_rel(age)*net_energy(fm = jm, age = age, oa = 0, scenario = scenario)) * successes)/attempts)
    
    metabolism = FMR_J_sea(jm) * metabolic_scalar
    growth = 0.06*((0.54*lipid_conversion) + (0.46*protein_conversion))
    
    new_mass = min(jm + ((energy_intake - metabolism - growth)*0.54/lipid_conversion) + 
                       ((energy_intake - metabolism - growth)*0.46/protein_conversion), 
                   maximum_mass)
    
    return(new_mass)
    
}

new_juvenile_mass_nursing <- function(jm, age, scenario = "base", metabolic_scalar = 1){
    maximum_mass = maximumMass(age = 730, class = "offspring")
    energy_required = (((maximum_mass - jm)*0.54)*lipid_conversion) + (((maximum_mass - jm)*0.46)*protein_conversion)
    
    milk_intake = ifelse(energy_required <= E_lact(jm, age)*0.95, max(energy_required, 0), E_lact(jm, age)*0.95)
    
    metabolism = FMR_J_land(jm) * metabolic_scalar
    growth = 0.06*((0.54*lipid_conversion) + (0.46*protein_conversion))
    
    new_mass = min(jm + 
                       ((milk_intake - metabolism)*0.54/lipid_conversion) + 
                       ((milk_intake - metabolism)*0.46/protein_conversion), 
                   maximum_mass) 
    return(new_mass)
    
}

new_juvenile_mass_supplemental <- function(jm, age, scenario = "base", metabolic_scalar = 1){
    maximum_mass = maximumMass(age = 730, class = "offspring")
    supplementation = ifelse(runif(1) > prob_food, 0, a_rel(age)*net_energy(fm = jm, age = age, oa = 0, scenario = scenario)*0.75)
    metabolism = FMR_J_sea(jm) * metabolic_scalar
    growth = 0.06*((0.54*lipid_conversion) + (0.46*protein_conversion))
    
    new_mass = min(jm + 
                       ((supplementation - metabolism - growth)*0.54/lipid_conversion) + 
                       ((supplementation - metabolism - growth)*0.46/protein_conversion),
                   maximum_mass)
    return(new_mass)
    
}

new_juvenile_mass_fasting <- function(jm, age, metabolic_scalar = 1, success_scalar = 1, diet_scalar = 1){
    metabolism = FMR_J_land(jm) * metabolic_scalar
    growth = 0.06*((0.54*lipid_conversion) + (0.46*protein_conversion))
    
    new_mass = max(jm -  
                       (((metabolism + growth)*0.95/lipid_conversion) + 
                            ((metabolism + growth)*0.05/protein_conversion)), 1)
    return(new_mass)
}




# Pups ----
new_pup_mass_weaned  <- function(pm, age){
    maximum_mass = maximumMass(age = 365, class = "offspring")
    
    metabolism = MR_pup(mass = pm, age = age)
    growth = 0.06*((0.54*lipid_conversion) + (0.46*protein_conversion))
    supplementation = a_rel(age)*(net_energy(fm = pm, age = age, scenario = scenario)*0.5)
    
    new_mass = min(max(pm - 
                           (((metabolism+growth-supplementation)*0.95/lipid_conversion) +
                                ((metabolism+growth-supplementation)*0.05/protein_conversion)), 1), 
                   maximum_mass)
    return(new_mass)
}

new_pup_mass_fasting <- function(pm, age){
    maximum_mass = maximumMass(age = 365, class = "offspring")
    
    metabolism = MR_pup(mass = pm, age = age)
    growth = 0.06*((0.54*lipid_conversion) + (0.46*protein_conversion))
    supplementation = a_rel(age)*(net_energy(fm = pm, age = age, scenario = scenario)*0.5)
    
    new_mass = min(max(pm - 
                           (((metabolism+growth-supplementation)*0.95/lipid_conversion) +
                                ((metabolism+growth-supplementation)*0.05/protein_conversion)), 1), 
                   maximum_mass)
    return(new_mass)
}

new_pup_mass_nursing <- function(pm, age, scenario = "base"){
    maximum_mass = maximumMass(age = 365, class = "offspring")
    energy_required = (((maximum_mass - pm)*0.54)*lipid_conversion) + (((maximum_mass - pm)*0.46)*protein_conversion)
    
    milk_intake = ifelse(energy_required < E_lact(pm, age)*0.95, 0, E_lact(pm, age)*0.95)
    
    metabolism = MR_pup(mass = pm, age = age)
    growth = 0.06*((0.54*lipid_conversion) + (0.46*protein_conversion))
    
    new_mass = min(pm + 
                       ((milk_intake - metabolism - growth)*0.54/lipid_conversion) + 
                       ((milk_intake - metabolism - growth)*0.46/protein_conversion), 
                   maximum_mass) 
    return(new_mass)
}
