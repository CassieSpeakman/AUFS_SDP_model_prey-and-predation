# Function to calculate the energy intake from foraging per day
# Author: CN Speakman
# Updated: August 2023

total_daily_energy_acquisition <- function(scenario = "default", oa = 0, om = 0, fa = 0, fm = 0, age){
    
    if (oa == 0){
        if(age < 3*365){
            metabolism = FMR_J_sea(aveMass(age))
            growth = 0.06*((0.54*lipid_conversion) + (0.46*protein_conversion))
            
            energy_required = metabolism+growth
            
        } else if(age >= 3*365){
            metabolism = FMR_sea(aveMass(age))
            gestation = E_gest(fa)
            growth = ifelse(age <= 4 *365, 0.035, ifelse(age > 4*365 & age <= 6*365, 0.02, ifelse(age > 6*365 & age <= 8*365, 0.011, ifelse(age > 8*365 & age < 10*365, 0.005, 0))))*blubber_conversion
            
            energy_required = metabolism+growth+gestation
            
        }
    } else if (oa > 0){
        maximum_mass = maximumMass(oa, class = "offspring")
        energy_required = (((maximum_mass - om)*0.54)*lipid_conversion) + (((maximum_mass - om)*0.46)*protein_conversion)
        lactation = E_lact(om, oa)
        pup_energy = ifelse(energy_required < lactation*0.95, 0, lactation)
        gestation = E_gest(fa)
        metabolism = FMR_sea(aveMass(age))
        growth = ifelse(age <= 4 *365, 0.035, ifelse(age > 4*365 & age <= 6*365, 0.02, ifelse(age > 6*365 & age <= 8*365, 0.011, ifelse(age > 8*365 & age < 10*365, 0.005, 0))))*blubber_conversion
        
        energy_required = metabolism+growth+pup_energy+gestation
        
    }
    
    return(energy_required)
}
