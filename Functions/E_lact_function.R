# Daily rate of milk consumption as a function of pup mass during maternal attendance periods
# Author: CN Speakman
# Updated: 25.03.2022

# Input (m) is the mass of the pup at time t.

# This calculates the mass-specific daily milk energy consumption (MJ kg-1 day-1) 
# per nursing day. Based on AUFS milk intake estimates (Arnould & Hindell 2002)


E_lact <- function(om, oa){
    milk_intake = ifelse(om < 6, 0, ifelse(om < 20, 48.234*log(om)-80.057, 48.234*log(20.1)-80.057))
    
    milk_delivered = milk_intake
    return(milk_delivered)
}
