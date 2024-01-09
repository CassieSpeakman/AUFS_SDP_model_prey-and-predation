nameScenario <- function(m, p, f){
  Mortality = ifelse(m == 1, "Base", ifelse(m == 2, "Ten", "Twenty"))
  Prob = ifelse(p == 1, "Base", ifelse(p == 2, "Reduced", "Increased"))
  Foraging = f
  
  Scenario = paste0("M_", Mortality, "_P_", Prob, "_FA_", Foraging)

  return(Scenario)
}
