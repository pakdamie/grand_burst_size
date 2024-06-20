### This is the model simulation to check how the                   
### This produces the necessary data files that are needed for
### further analysis                                         
library(here)

# This is the main simulations of Plasmodium
# chabaudi and Plasmodium falciparum assuming nothing has changed
# about the initial red blood cell density or the replenishment rate.
# This is one of the longer code to source
source(here("Code", "Functions", "FUNC_Package_Loader.R"))
source(here("Code","Functions","FUNC_M_Sourcing_Simulation.R"))
source(here("Code", "Functions", "FUNC_M_Find_OptimalBV_Group.R"))

ifelse(dir.exists(here("Output/Full_Model")) == FALSE,
       dir.create("Output/Full_Model"),
       "Directory exists already"
)

ifelse(dir.exists(here("Output/Fitness_Model")) == FALSE,
       dir.create("Output/Fitness_Model"),
       "Directory exists already"
)

Fitness_MODEL_PC_muM <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='mu_M')
Fitness_MODEL_PF_muM <- Simulate_Infection("PF",C_V_specific = 0.1, variable_interest ='mu_M')

Fitness_MODEL_PC_alpha1 <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='alpha1')
Fitness_MODEL_PF_alpha1 <- Simulate_Infection("PF",C_V_specific = 0.1, variable_interest ='alpha1')


xFitness_MODEL_PC_pmax <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='pmax')
Fitness_MODEL_PF_pmax <- Simulate_Infection("PF",C_V_specific = 0.1, variable_interest ='pmax')

Fitness_MODEL_PC_R <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='R_Modifier')
Fitness_MODEL_PF_R <- Simulate_Infection("PF",C_V_specific = 0.1, variable_interest ='R_Modifier')

Fitness_PC_muM <- Find_OptimalBV_Group(Fitness_MODEL_PC_muM, "mu_M")
Fitness_PF_muM <- Find_OptimalBV_Group(Fitness_MODEL_PF_muM, "mu_M")
Fitness_PC_muM$species <- "PC"
Fitness_PF_muM$species <- "PF"
Fitness_muM <- rbind(Fitness_PC_muM ,Fitness_PF_muM) 

Fitness_PC_alpha1 <- Find_OptimalBV_Group(Fitness_MODEL_PC_alpha1, "alpha_1")
Fitness_PF_alpha1 <- Find_OptimalBV_Group(Fitness_MODEL_PF_alpha1, "alpha_1")
Fitness_PC_alpha1$species <- "PC"
Fitness_PF_alpha1$species <- "PF"
Fitness_alpha1 <- rbind(Fitness_PC_alpha1 ,Fitness_PF_alpha1) 

Fitness_PC_pmax<- Find_OptimalBV_Group(Fitness_MODEL_PC_pmax, "p_val")
Fitness_PF_pmax <- Find_OptimalBV_Group(Fitness_MODEL_PF_pmax, "p_val")
Fitness_PC_pmax$species <- "PC"
Fitness_PF_pmax$species <- "PF"
Fitness_pmax <- rbind(Fitness_PC_pmax ,Fitness_PF_pmax) 


Fitness_PC_R<- Find_OptimalBV_Group(Fitness_MODEL_PC_R, 'R_Modifier')
Fitness_PF_R <- Find_OptimalBV_Group(Fitness_MODEL_PF_R,'R_Modifier')
Fitness_PC_R$species <- "PC"
Fitness_PF_R$species <- "PF"
Fitness_R <- rbind(Fitness_PC_R ,Fitness_PF_R) 

Plot_Simulation(Fitness_muM, "Mortality rate", "mu_M") 
Plot_Simulation(Fitness_pmax, "Infection matrix", "p_val")
Plot_Simulation(Fitness_R, "R_Modifier", "R_Modifier")

Plot_Simulation(Fitness_alpha1, "alpha_1", "alpha_1") + geom_vline(xintercept = 1)

