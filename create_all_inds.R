# Summarise the data:
all_inds <- lapply(Ind, t)
all_inds <- lapply(all_inds, data.frame)
all_inds <- dplyr::bind_rows(all_inds, .id = "Ind")
all_inds$Ind <- as.numeric(paste(all_inds$Ind))

# Assign column names to array:
colnames(all_inds)[2:14] <- c("age", "mass","state","location","fetusAge","pupMass","pupAge","juvMass","juvAge","cumOff", "decision", "metabolicScalar", "foragingDays")
all_inds$Age <- floor(all_inds$age/365) # Age in years

# Assign the reproductive states:
all_inds <- all_inds %>% group_by(Ind) %>%
  dplyr::mutate(State = ifelse(state == 1, "Non-reproductive", 
                               ifelse(state == 2, "Pregnant", 
                                      ifelse(state == 3, "Nursing pup", 
                                             ifelse(state == 4, "Nursing juv",
                                                    ifelse(state == 5, "Pregnant + Nursing pup", 
                                                           ifelse(state == 6, "Pregnant + Nursing juv",
                                                                  ifelse(state == 0, "Deceased",
                                                                         ifelse(state == -1, "Starvation", 
                                                                                ifelse(state == -2, "Mortality", NA))))))))))

# Assign the decisions:
all_inds <- all_inds %>% group_by(Ind) %>%
  dplyr::mutate(Decisions = ifelse(decision == -2, "Mortality", 
                                   ifelse(decision == -1, "Starvation",
                                          ifelse(decision == -23, "Starvation Terminated & Ceased juvenile",
                                                 ifelse(decision == -24, "Starvation Terminated & Recruit juvenile",
                                                        ifelse(decision == -20, "Starvation Terminated & Ceased pup",
                                                               ifelse(decision == -21, "Starvation Terminated & Ceased pup",
                                                                      ifelse(decision == -22, "Starvation Terminated & Recruit pup",
                                                                             ifelse(decision == -18, "Mortality Terminated & Ceased juvenile",
                                                                                    ifelse(decision == -19, "Mortality Terminated & Recruit juvenile",
                                                                                           ifelse(decision == -12, "Mortality Terminated & Ceased pup",
                                                                                                  ifelse(decision == -13, "Mortality Terminated & Ceased pup",
                                                                                                         ifelse(decision == -14, "Mortality Terminated & Recruit pup",
                                                                                                                ifelse(decision == -8, "Mortality Ceased juvenile",
                                                                                                                       ifelse(decision == -9, "Mortality Recruit juvenile",
                                                                                                                              ifelse(decision == -4, "Mortality Ceased pup",
                                                                                                                                     ifelse(decision == -5, "Mortality Ceased pup",
                                                                                                                                            ifelse(decision == -6, "Mortality Recruit pup",
                                                                                                                                                   ifelse(decision == -3, "Mortality Terminated",
                                                                                                                                                          ifelse(decision == 1, "Implant", 
                                                                                                                                                                 ifelse(decision == 2, "Abort",
                                                                                                                                                                        ifelse(decision == 3, "Birth", 
                                                                                                                                                                               ifelse(decision == 4, "Abandon pup",
                                                                                                                                                                                      ifelse(decision == 5, "Wean pup", 
                                                                                                                                                                                             ifelse(decision == 6, "Recruit pup",
                                                                                                                                                                                                    ifelse(decision == 7, "Nurse as juvenile",
                                                                                                                                                                                                           ifelse(decision == 8, "Wean juvenile", 
                                                                                                                                                                                                                  ifelse(decision == 9, "Recruit juvenile",
                                                                                                                                                                                                                         ifelse(decision == 10, "Birth & Wean pup", 
                                                                                                                                                                                                                                ifelse(decision == 11, "Birth & Recruit pup",
                                                                                                                                                                                                                                       ifelse(decision == 12, "Abort & Abandon pup",
                                                                                                                                                                                                                                              ifelse(decision == 13, "Abort & Wean pup", 
                                                                                                                                                                                                                                                     ifelse(decision == 14, "Abort & Recruit pup",
                                                                                                                                                                                                                                                            ifelse(decision == 15, "Abort & Nurse as juvenile",
                                                                                                                                                                                                                                                                   ifelse(decision == 16, "Birth & Wean juvenile",
                                                                                                                                                                                                                                                                          ifelse(decision == 17, "Birth & Recruit juvenile",  
                                                                                                                                                                                                                                                                                 ifelse(decision == 18, "Abort & Wean juvenile",
                                                                                                                                                                                                                                                                                        ifelse(decision == 19, "Abort & Recruit juvenile", 
                                                                                                                                                                                                                                                                                               NA))))))))))))))))))))))))))))))))))))))


# Assign state transitions:
all_inds <- all_inds %>% group_by(Ind) %>%
  dplyr::mutate(Time = 1:length(Ind), 
                Year = ceiling(Time/365)+yearsCompletedTotal, # Time in years
                rlid = data.table::rleid(location))