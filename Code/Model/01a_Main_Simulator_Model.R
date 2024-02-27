###############################################################
### This produces the necessary data files that is needed for##
### further analysis                                         ##
##############################################################

library(here)

# This is the main simulation assuming nothing has changed
# about the initial red blood cell density or the replenishment rate.
# This is one of the longer code to source

### Packages to load
source(here("Code", "Helper_Function_Code", "FUNC_00_Packages_Loader.R"))
### Main modeling code
sourceCpp(here("Code", "RCPP_Code", "rcpp_malaria_dynamics_CUT.cpp"))
sourceCpp(here("Code", "RCPP_Code", "rcpp_malaria_dynamics_UNCUT.cpp"))
source(here("Code", "Simulator_Code", "Simulator_Main_2.R"))
source(here("Code", "Helper_Function_Code", "FUNC_00_Fitness_Functions.R"))
source(here("Code", "Helper_Function_Code", "FUNC_02_Simulator_Code.R"))

ifelse(dir.exists(here("Output/Full_Model")) == FALSE,
  dir.create("Output/Full_Model"),
  "Directory exists already"
)

ifelse(dir.exists(here("Output/Fitness_Model")) == FALSE,
  dir.create("Output/Fitness_Model"),
  "Directory exists already"
)

FULL_MODEL_SUPP_SmallInoc <- FULL_MODEL_SIMULATING_Duration(43.85965, 48, "low") 
FULL_MODEL_MedINoc <- FULL_MODEL_SIMULATING_Duration(4385.96491,48, 'med')
FULL_MODEL_SUPP_HighInoc <-FULL_MODEL_SIMULATING_Duration(438596.49123, 48, 'high')


###Proceed to 03_MAIN_ANALYSIS_SURFACEPLOTS.R to make the full surfaceplots


