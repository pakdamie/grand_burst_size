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
source(here("Code", "Functions", "FUNC_M_Plot_VariableInterestR.R"))

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

Fitness_MODEL_PC_pmax <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='pmax')
Fitness_MODEL_PF_pmax <- Simulate_Infection("PF",C_V_specific = 0.1, variable_interest ='pmax')

Fitness_MODEL_PC_R <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='R_Modifier')
Fitness_MODEL_PF_R <- Simulate_Infection("PF",C_V_specific = 0.1, variable_interest ='R_Modifier')


###Mortality
Fitness_mu_M_All <- Rescale_Rate_Variable_Interest(Fitness_MODEL_PC_muM ,
                               Fitness_MODEL_PF_muM, "mu_M")

Fitness_mu_M_All$mu_M_lifespan <- (1/Fitness_mu_M_All$mu_M_rescale) * 1440

mu_M_GG <- ggplot(Fitness_mu_M_All, aes(
  x = mu_M_lifespan,
  y = B_V, group = species, color = species
)) +
  geom_line( plot_layout(guides = 'collect', ncol = 2)) +
  geom_point(
    data = subset(
      Fitness_mu_M_All,
      Fitness_mu_M_All$controlornot == "control"
    ),
    aes(color = species), size = 3
  ) +
  scale_color_manual(name = "", values = c("#2e2d84", "#ee4493")) +
  theme_classic() +
  xlab("Merozoite lifespan (minutes)") +
  ylab("Optimal burst size") +
  theme(
    axis.text = element_text(color = "black", size = 13),
    axis.title = element_text(color = "black", size = 14)
  ) + ylim(0,100)

Fitness_pmaxAll <- Rescale_Rate_Variable_Interest(Fitness_MODEL_PC_pmax ,
                                                   Fitness_MODEL_PF_pmax, "p_val")


pmax_GG <- ggplot(Fitness_pmaxAll, aes(
  x = p_val_rescale,
  y = B_V, group = species, color = species
)) +
  geom_line(linewidth = 0.9) +
  geom_point(
    data = subset(
      Fitness_pmaxAll,
      Fitness_pmaxAll$controlornot == "control"
    ),
    aes(color = species), size = 3
  ) +
  scale_color_manual(name = "", values = c("#2e2d84", "#ee4493")) +
  theme_classic() +
  xlab("Invasion rate") +
  ylab("Optimal burst size") +
  theme(
    axis.text = element_text(color = "black", size = 13),
    axis.title = element_text(color = "black", size = 14)
  ) + ylim(0,100)


mu_M_GG + pmax_GG +  plot_layout(guides = 'collect')

ggsave(here("Figures", "Raw", "Variable_Interest_mu_M_pmax.pdf"),
       height = 5,
       width = 10, units = "in"
)

Fitness_R_Modifier_All<- Rescale_Rate_Variable_Interest(Fitness_MODEL_PC_R ,
                                                  Fitness_MODEL_PF_R, "R_Modifier")



ggplot(Fitness_R_Modifier_All, aes(
  x = R_Modifier_rescale,
  y = B_V, group = species, color = species
)) +
  geom_line(linewidth = 0.9) +
  geom_point(
    data = subset(
      Fitness_R_Modifier_All,
      Fitness_R_Modifier_All$controlornot == "control"
    ),
    aes(color = species), size = 3
  ) +
  scale_color_manual(name = "", values = c("#2e2d84", "#ee4493")) +
  theme_classic() +
  xlab("Initial red blood cells") +
  ylab("Optimal burst size") +
  theme(
    axis.text = element_text(color = "black", size = 13),
    axis.title = element_text(color = "black", size = 14)
  ) + ylim(0,100)

ggsave(here("Figures", "Raw", "Variable_Interest_InitialRBC.pdf"),
       height = 5,
       width = 6, units = "in"
)
Fitness_alpha_1_All<- Rescale_Rate_Variable_Interest(Fitness_MODEL_PC_alpha1 ,
                                                        Fitness_MODEL_PF_alpha1 , "alpha_1")


ggplot(Fitness_alpha_1_All, aes(
  x = alpha_1_rescale,
  y = B_V, group = species, color = species
)) +
  geom_line(linewidth = 0.9) +
  geom_point(
    data = subset(
      Fitness_alpha_1_All,
      Fitness_alpha_1_All$controlornot == "control"
    ),
    aes(color = species), size = 3
  ) +
  scale_color_manual(name = "", values = c("#2e2d84", "#ee4493")) +
  theme_classic() +
  xlab("Asexual cycle (days)") +
  ylab("Optimal burst size") +
  theme(
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(color = "black", size = 15)
  ) + ylim(0,100)
ggsave(here("Figures", "Raw", "Variable_Interest_alpha_1.pdf"),
       height = 5,
       width = 6, units = "in"
)

