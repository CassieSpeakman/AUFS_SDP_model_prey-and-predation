# Digestive efficiency
# Author: CN Speakman
# Updated: August 2023

# Energy loss during digestion comes through three avenues: 
#  - fecal energy loss (FEL), as a proportion of the gross energy intake (GEI)
#  - urinary energy loss (UEL), as a proportion of the digestible energy (DE), 
#         resulting in the metabolisable energy (ME)
#  - heat increment of feeding (HIF)

# Input (FEL) is the fecal energy loss.
# Default to 0.04 as estimated from values in Gomez et al 2016

# Input (UEL) is the urinary energy loss.
# Default to 0.13 as estimated from values in Gomez et al 2016

# Input (HIF) is the heat increment of feeding.
# Default to 6.3 as estimated from values in Gomez et al 2016

# Input (fm) is the mass of the female

net_energy <- function(FEL=0.04, UEL=0.13, oa = 0, om = 0, fa = 0, fm, age, scenario, success_scalar = 1, diet_scalar = 1){
    GEI = total_daily_energy_acquisition(scenario = scenario, oa = oa, om = om, fa = fa, fm = fm, age = age) 
    FEL = FEL*GEI 
    DE = GEI - FEL 
    UEL = UEL*DE 
    ME = DE - UEL 
    net_Energy = ME # If not using DLW metabolic estimates, need to include HIF; HIF is incorporated into DLW estimates
    
    multiplier = ifelse(oa == 0, 1.5, 1.2) # Originally 1.7 and 1.2
    energy_acquired = net_Energy*multiplier
    return(energy_acquired)
}
