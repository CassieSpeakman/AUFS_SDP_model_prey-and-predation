convert_mlo2_mj <- function(ave_mass, mlo2){ # for ml O2 /kg /min
  mlo2_day = mlo2*60*24 # convert to days
  lo2_day = mlo2_day/1000 # convert to LO2
  kcal_day = lo2_day*4.8 # convert to kcal
  kj_day = kcal_day*4.184 # convert to kJ
  mj_day = kj_day/1000 # convert to MJ
  mass_conversion = (mj_day*ave_mass)/(ave_mass^0.75) # MJ d-1 kg^-0.75
  mass_conversion
}

convert_mlo2_h_mj <- function(ave_mass, mlo2){ # for ml O2 /g /h
  mlo2_day = mlo2*24 # convert to days
  lo2_day = mlo2_day/1000 # convert to LO2
  kcal_day = lo2_day*4.8 # convert to kcal
  kj_day = kcal_day*4.184 # convert to kJ
  mj_day = kj_day/1000 # convert to MJ
  mass_conversion = (mj_day*(ave_mass*1000))/(ave_mass^0.75) # MJ d-1 kg^-0.75
  mass_conversion
}

convert_watts_mj <- function(ave_mass, watts){
  j_day = watts*60*60*24 # convert to days; 1 W = 1 j/s
  mj_day = j_day*10^-6 # convert to MJ
  mass_conversion = (mj_day*ave_mass)/(ave_mass^0.75) # MJ d-1 kg^-0.75
  mass_conversion
}


# 1 W = 1 J/s
# So convert to mins, hours, day, then 
# convert J to MJ (i.e. J/1000, KJ/1000)
# This value is then put ^0.75 to fix the
# conversion factor. 
# This is the FMR in MJ/d
# From there, multiply the FMR by mass^0.75
# for the FMR in MJ/d/kg
#	Conversion: 4.8 kcal/LO2 and 4.184 kJ/kcal



