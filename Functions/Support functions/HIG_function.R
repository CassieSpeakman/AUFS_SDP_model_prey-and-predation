HIG <- function(t){
  # heat_increment_gestation = ((1*10^-7)*t^2)+(0.0012*t)-0.002
  # return(heat_increment_gestation) # HIG in MJ/d
  
  # # HIG = 4400 kcal x birth mass^1.2
  # HIG_multiplier = (4400*0.004187)*(7.295^1.2) 
  # # 4400 * 0.004187 converts from kcal to MJ
  # 
  # Daily_mass <- foetal_growth_function(t) + placental_growth_function(t)
  # Total_mass <- sum(foetal_growth_function(1:245)) + sum(placental_growth_function(1:245))
  # (Daily_mass/Total_mass)*HIG_multiplier
  
  ifelse(t<75, 0, ((1*10^-7)*t^3)+((4*10^-5)*t^2)-(0.0046*t)+0.0735)
}
