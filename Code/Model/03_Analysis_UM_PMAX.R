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
source(here("Code", "Functions","FUNC_M_Recalculate_scale_variable_interest.R"))

ifelse(dir.exists(here("Output/Full_Model")) == FALSE,
       dir.create("Output/Full_Model"),
       "Directory exists already"
)

ifelse(dir.exists(here("Output/Fitness_Model")) == FALSE,
       dir.create("Output/Fitness_Model"),
       "Directory exists already"
)


Fitness_MODEL_PC_combo <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='combo')
Fitness_MODEL_PF_combo <- Simulate_Infection("PF", C_V_specific = 0.1, variable_interest ='combo')

Fitness_MODEL_PC_alpha1 <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='alpha1')
Fitness_MODEL_PF_alpha1 <- Simulate_Infection("PF",C_V_specific = 0.1, variable_interest ='alpha1')


Fitness_MODEL_PC_R <- Simulate_Infection("PC", C_V_specific = 0.1, variable_interest ='R_Modifier')
Fitness_MODEL_PF_R <- Simulate_Infection("PF",C_V_specific = 0.1, variable_interest ='R_Modifier')

###Surface plot

Fitness_combo_Modifier_All <- Rescale_Rate_Variable_Interest(Fitness_MODEL_PC_combo,
                                                         Fitness_MODEL_PF_combo, 
                                                         "combo")

Time_mod <-  c("1\nmin", "5\nmins", "7.2\nmins", "30\nmins", "1\nhr", "1\nday", "1 \nweek")
Multiplier <-  c("1%", "25%", "50%", "Control", "125%", "150%")


PC_combo_GG <- ggplot(subset(Fitness_combo_Modifier_All,
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


PF_combo_GG <- ggplot(subset(Fitness_combo_Modifier_All,
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


PC_combo_GG + plot_spacer()+ PF_combo_GG + plot_layout(guides = "collect",
                                                       widths=c(4,0.5,4))+
        plot_annotation(tag_levels = 'A')

ggsave(here("Figures","Raw", "UM_PMAX_Surfaceplots_PC_PF.pdf"), width = 13,
       height =4, units = "in")

### 

Fitness_R_Modifier_All <- Rescale_Rate_Variable_Interest(Fitness_MODEL_PC_R ,
                                          Fitness_MODEL_PF_R, "R_Modifier")


R_Modifier_GG <- plot_PC_PF_comparison (Fitness_mu_M_All, "Initial RBC density");
R_Modifier_GG + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x))) 



ggsave(here("Figures",  "Variable_Interest_InitialRBC.pdf"),
       height = 5,
       width = 6, units = "in"
)








###Asexual development time

Fitness_alpha_1_All <- Rescale_Rate_Variable_Interest(Fitness_MODEL_PC_alpha1 ,
                                                        Fitness_MODEL_PF_alpha1 , 
                                                       "alpha_1")


alpha1_GG <- ggplot(Fitness_alpha_1_All, aes(
  x = alpha_1_rescale,
  y = B_V, group = species, color = species,
  linetype = species
)) +
  geom_line(linewidth = 0.8) +
  geom_point(
    data = subset(
      Fitness_alpha_1_All,
      Fitness_alpha_1_All$controlornot == "control"
    ),
    aes(color = species), size = 3
  ) +
  theme_classic() +
  xlab("Asexual cycle (days)") +
  ylab("Optimal burst size") +
  theme(legend.position = 'top',
    axis.text = element_text(color = "black", size = 14),
    axis.title = element_text(color = "black", size = 15)
  ) + ylim(0,100)
ggsave(here("Figures", "Raw", "Variable_Interest_alpha_1.pdf"),
       height = 5,
       width = 6, units = "in"
)


