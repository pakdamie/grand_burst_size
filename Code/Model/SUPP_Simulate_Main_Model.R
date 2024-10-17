### This is the original model simulation.                    
### This produces the necessary data files that are needed for
### further analysis                                         
library(here)

# This is the main simulations of Plasmodium
# chabaudi and Plasmodium falciparum assuming nothing has changed
# about the initial red blood cell density or the replenishment rate.
# This is one of the longer code to source

source(here("Code","Functions","FUNC_M_Sourcing_Simulation.R"))


ifelse(dir.exists(here("Output/Full_Model")) == FALSE,
  dir.create("Output/Full_Model"),
  "Directory exists already"
)

ifelse(dir.exists(here("Output/Fitness_Model")) == FALSE,
  dir.create("Output/Fitness_Model"),
  "Directory exists already"
)
###Default argument: 
Fitness_MODEL_PC  <- Simulate_Infection("PC")
Fitness_MODEL_PF <- Simulate_Infection("PF")



