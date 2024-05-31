### Packages to load
source(here("Code", "Functions", "FUNC_Package_Loader.R"))
### Main modeling code
source(here("Code", "Model", "simulator_code", "Simulator_Main_2_PC.R"))
source(here("Code", "Model", "simulator_code", "Simulator_Main_2_PF.R"))

#The main function
source(here("Code", "Functions", "FUNC_M_Simulate_Infection.R"))

#Calculates fitness
source(here("Code", "Functions", "FUNC_M_Calculate_Fitness.R"))
#Determines the acute phase length
source(here("Code", "Functions", "FUNC_M_Determine_Acute_Phase_Length.R"))
#Calculates the time-varying RM
source(here("Code", "Functions", "FUNC_M_Calculate_RM.R"))
#Calculates the initial RM to see if the infection establishes
source(here("Code", "Functions", "FUNC_M_Calculate_Initial_RM.R"))

