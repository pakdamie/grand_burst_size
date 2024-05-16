###############################################################
### This produces the necessary data files that is needed for##
### further analysis                                         ##
##############################################################

library(here)

# This is the main simulations of Plasmodium
# chabaudi and Plasmodium falciparum assuming nothing has changed
# about the initial red blood cell density or the replenishment rate.
# This is one of the longer code to source

library(here)
### Packages to load
source(here("Code", "Functions", "FUNC_Package_Loader.R"))
### Main modeling code
source(here("Code", "Model", "simulator_code", "Simulator_Main_2_PC.R"))
source(here("Code", "Model", "simulator_code", "Simulator_Main_2_PF.R"))
source(here("Code", "Functions", "FUNC_00_Fitness_Functions.R"))
source(here("Code", "Functions", "FUNC_Model_Simulator_Code.R"))

ifelse(dir.exists(here("Output/Full_Model")) == FALSE,
  dir.create("Output/Full_Model"),
  "Directory exists already"
)

ifelse(dir.exists(here("Output/Fitness_Model")) == FALSE,
  dir.create("Output/Fitness_Model"),
  "Directory exists already"
)
Fitness_MODEL_PC  <- FULL_MODEL_SIMULATING_Duration(4385.965,"PC", "Yes")
Fitness_MODEL_PF <- FULL_MODEL_SIMULATING_Duration(25000, "PF", "Yes")



