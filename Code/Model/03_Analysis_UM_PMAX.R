### This is the model simulation to check how the                   
### This produces the necessary data files that are needed for
### further analysis                                         
library(here)

# This is the main simulations of Plasmodium
# chabaudi and Plasmodium falciparum assuming nothing has changed
# about the initial red blood cell density or the replenishment rate.
# This is one of the longer code to source

source(here("Code","Functions","FUNC_M_Sourcing_Simulation.R"))
source(here("Code","Functions","Fitness_MODEL_PC_pmax.R"))

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

Fitness_MODEL_PC_pmax <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='pmax')
Fitness_MODEL_PF_pmax <- Simulate_Infection("PF",C_V_specific = 0.1, variable_interest ='pmax')



Fitness_PC_muM<- Find_OptimalBV_Group(Fitness_MODEL_PC_muM, "mu_M")
Fitness_PF_muM <- Find_OptimalBV_Group(Fitness_MODEL_PF_muM, "mu_M")


Fitness_PC_pmax<- Find_OptimalBV_Group(Fitness_MODEL_PC_pmax, "p_val")
Fitness_PF_pmax <- Find_OptimalBV_Group(Fitness_MODEL_PF_pmax, "p_val")

Fitness_PC_muM$M_longevity <- 1/Fitness_PC_muM$mu_M
Fitness_PF_muM$M_longevity <- 1/Fitness_PF_muM$mu_M

ggplot(subset(Fitness_PC_muM,Fitness_PC_muM$M_longevity < 10),
              
              aes( x=1/mu_M, y = B_V )) + geom_line(linetype= 1)+ 
geom_line(data = subset(Fitness_PF_muM,Fitness_PF_muM$M_longevity < 10), aes( x= 1/mu_M, y = B_V ), linetype = "dashed")+
  xlab("Merozoite mortality") + ylab("Optimal burst size") + theme_classic()+
  theme()

ggplot(Fitness_PC_pmax, aes( x= p_val, y = B_V ))+geom_line(color = 'blue')+ 
  geom_line(data = Fitness_PF_pmax , aes( x= p_val, y = B_V ),color = 'red')
 