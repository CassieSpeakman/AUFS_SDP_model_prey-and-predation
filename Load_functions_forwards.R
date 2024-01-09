# Load functions and parameters ----
library(here)

source(here("Functions/E_lact_function.R"))
source(here("Functions/E_gest_function.R"))
source(here("Functions/E_prey_baseline.R"))
source(here("Functions/Metabolism_female_functions.R"))
source(here("Functions/Metabolism_juv_functions.R"))
source(here("Functions/Metabolism_pup_functions.R"))
source(here("Functions/Digestive_efficiency_function.R"))
source(here("Functions/New_mass_functions_forwards.R"))
source(here("Functions/Juvenile_efficiency_function_on.R"))
source(here("Functions/Mass_thresholds.R"))
source(here("Functions/Fat_mass_conversion.R"))
source(here("Functions/Recruitment_function.R"))
source(here("Functions/Mortality_function.R"))
source(here("Functions/Set_population.R"))
source(here("Functions/Name_scenario.R"))

# Dates and times for state transitions _----------------------------------------
tMax = 21*365
yearSenescence = 20*365
yearOne = 3*365
yearTwo = 4*365
yearThree = 5*365

# Metabolic parameters ----------------------------------------------------------
blubber_conversion = 20.7  # MJ/kg - energy density of blubber from Liwanag 2012
lipid_conversion = 39.3  # MJ/kg - energy density of lipid (blubber & structural tissue) (Kuhnleini & Soueida 1992)
protein_conversion = 18.0  # MJ/kg - energy density of protein (Kuhnleini & Soueida 1992)
placental_conversion = 3.3 # MJ/kg; as much as 4.7 MJ/kg

pup_masses = rnorm(n = 1000, mean = 7.6, sd = 0.1)