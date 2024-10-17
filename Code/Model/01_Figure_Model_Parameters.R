### This is the model simulation to check how the                   
### This produces the necessary data files that are needed for
### further analysis                                         
library(here)

# This is the main simulations of Plasmodium
# chabaudi and Plasmodium falciparum assuming nothing has changed
# about the initial red blood cell density or the replenishment rate.
# This is one of the longer code to source

source(here("Code", "Functions", "FUNC_Package_Loader.R"))
source(here("Code","Functions",  "FUNC_M_Sourcing_Simulation.R"))
source(here("Code", "Functions", "FUNC_M_Find_OptimalBV_Group.R"))
source(here("Code", "Functions", "FUNC_M_Plot_VariableInterestR.R"))
source(here("Code", "Functions", "FUNC_M_Recalculate_scale_variable_interest.R"))

ifelse(dir.exists(here("Output/Full_Model")) == FALSE,
       dir.create("Output/Full_Model"),
       "Directory exists already"
)

ifelse(dir.exists(here("Output/Fitness_Model")) == FALSE,
       dir.create("Output/Fitness_Model"),
       "Directory exists already"
)

###Merozoite mortality and invasion rate
Fitness_MODEL_PC_combo <- Simulate_Infection(
  "PC", C_V_specific = 0.10,
  variable_interest ='combo')

Fitness_MODEL_PF_combo <- Simulate_Infection(
  "PF", C_V_specific = 0.10, 
  variable_interest ='combo')

###Alpha duration
Fitness_MODEL_PC_alpha1 <- Simulate_Infection(
  "PC", C_V_specific = 0.10, 
  variable_interest ='alpha1')

Fitness_MODEL_PF_alpha1 <- Simulate_Infection(
  "PF",C_V_specific = 0.1, 
  variable_interest ='alpha1')

#Initial red blood cell availability
Fitness_MODEL_PC_R <- Simulate_Infection(
  "PC", C_V_specific = 0.10, 
  variable_interest ='R_Modifier')
Fitness_MODEL_PF_R <- Simulate_Infection(
  "PF", C_V_specific = 0.10, 
  variable_interest ='R_Modifier')

# Surface plots for both the merozoite mortality 
# and invasion rate

Fitness_combo_Modifier_All <- Rescale_Rate_Variable_Interest(
  Fitness_MODEL_PC_combo, Fitness_MODEL_PF_combo, "combo")

Time_mod <-  c("1\nmin", "5\nmins", "7.2\nmins", 
               "30\nmins", "1\nhr", "1\nday", "1 \nweek")
Multiplier <-  c("1%", "25%", "50%", "Control", "125%", "150%")


PC_combo_GG <- ggplot(
  subset(Fitness_combo_Modifier_All,
  Fitness_combo_Modifier_All$species == "P.chabaudi"),
  aes(x = as.factor(mu_M_rescale), 
      y = as.factor(p_val), 
  fill= log10(B_V)))+
  geom_tile(color = 'black', size = 0.6)+
  annotate("rect", xmin= 4-0.5, xmax = 4+0.5, ymin =4 - 0.5, ymax =4+0.5, 
           colour = 'black', fill = NA, size = 2)+
  scale_fill_viridis(name = "Burst size",option = 'inferno' , limit= c(0,2)) +
  scale_x_discrete(expand=c(0,0),label = Time_mod)+
  scale_y_discrete(expand=c(0,0), label = Multiplier)+
  xlab("Merozoite Lifespan")+
  ylab("Invasion multiplier")+
  coord_equal()+
  theme_classic()+
  theme(panel.background = element_rect(fill = 'white'),
        axis.text = element_text(size = 14,color = 'black'),
        axis.title = element_text(size = 15, color = 'black'),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 1.5)) 

PF_combo_GG <- ggplot(
  subset(Fitness_combo_Modifier_All,
    Fitness_combo_Modifier_All$species == "P.falciparum"),
  aes(x = as.factor(mu_M_rescale), 
    y = as.factor(p_val), 
    fill= log10(B_V)))+
  geom_tile(color = 'black', size = 0.6)+
  annotate("rect", xmin= 3-0.5, xmax = 3+0.5, ymin =4 - 0.5, ymax =4+0.5, 
    colour = 'black', fill = NA, size = 2)+
  scale_fill_viridis(option = 'inferno' ,
    name = "Burst size",limit= c(0,2)) +
  scale_x_discrete(drop = FALSE,label = Time_mod)+
  scale_y_discrete(expand=c(0,0), label = Multiplier)+
  xlab("Merozoite Lifespan")+
  ylab("Invasion multiplier")+
  coord_equal()+
  theme_classic()+
  theme(panel.background = element_rect(fill = 'white'),
    axis.text = element_text(size = 14,color = 'black'),
    axis.title = element_text(size = 15, color = 'black'),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 1.5))

PC_combo_GG + plot_spacer() + PF_combo_GG + 
  plot_layout(guides = "collect", widths=c(4,0.5,4)) +
  plot_annotation(tag_levels = 'A')

ggsave(here("Figures","Raw", 
  "UM_PMAX_Surfaceplots_PC_PF.pdf"), 
  width = 13, height = 4, units = "in")

### 

Fitness_R_Modifier_All <- Rescale_Rate_Variable_Interest(
  Fitness_MODEL_PC_R, Fitness_MODEL_PF_R, "R_Modifier")

model_RBC_GG <- ggplot(
  Fitness_R_Modifier_All,
  aes(x = 
    (R_Modifier_rescale), 
    y = B_V,
    group = species,
    linetype = species)) + 
  geom_line(size = 0.6) +
  geom_point(data = subset(Fitness_R_Modifier_All,
    Fitness_R_Modifier_All$controlornot == 'control'),
    aes(x =(R_Modifier_rescale), y = B_V)) +
  scale_linetype_manual(name = '', values = c(1,2)) +
  scale_x_log10(
    limits = c(10^6.25, 10^7.75),
    breaks = 10^seq(6.25, 7.75, by = 0.25),  # From 10^5 to 10^8 in steps of 0.5
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_continuous(limits = c(0,100))+
  xlab(expression("Initial RBCs ("*mu*"L"^-1*")")) +
  ylab("Optimal burst size") +
  theme_classic() + 
  theme(axis.text  = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'),
        legend.position = c(0.80,0.95))


data_RBC_GG + model_RBC_GG 

ggsave(here("Figures","Raw", 
            "R_Modifier_Scatter_PC_PF.pdf"), 
       width = 10, height = 5, units = "in")



###Asexual development time

Fitness_alpha_1_All <- Rescale_Rate_Variable_Interest(
  Fitness_MODEL_PC_alpha1, 
  Fitness_MODEL_PF_alpha1, 
  "alpha_1")


alpha1_GG <- ggplot(
  Fitness_alpha_1_All, aes(
  x = alpha_1_rescale,
  y = B_V, group = species,
  linetype = species)) +
  geom_line(size = 0.6) +
  geom_point(data = subset(Fitness_alpha_1_All,
    Fitness_alpha_1_All$controlornot == 'control'),
    aes(x =(alpha_1_rescale), y = B_V)) +
  scale_linetype_manual(name = '', values = c(1,2)) +
  scale_y_continuous(limits = c(0,100))+
  xlab(expression("Asexual cycle duration (days)")) +
  ylab("Optimal burst size") + 
  theme_classic() + 
  theme(axis.text  = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'),
        legend.position = c(0.80,0.95))



duration_burst_size_GG + alpha1_GG

ggsave(here("Figures","Raw", 
            "alpha1_Scatter_PC_PF.pdf"), 
       width = 10, height = 5, units = "in")



duration_burst_size_GG + alpha1_GG 
