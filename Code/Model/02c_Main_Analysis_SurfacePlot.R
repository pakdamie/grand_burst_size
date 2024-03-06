# HEADER --------------------------------------------
#
# Author: Damie Pak
# Copyright (c) Damie Pak, 2023
# Email:  dp495@cornell.edu
#
# Date: 2023-04-21
#
# Script Name: 03_Main_Analysis_SurfacePlot.R
#
# Script Description: This is the script that takes the output
#from the previous analysis and produce the first surface plot 
#
# Notes: Originally, had the fitness and duration plot together,
# but changed it so that only the fitness is in main and the
# duration is in the supp.
#

################################
### Packages and Code to load###
################################
library(here)
source(here("Code", "Functions", "FUNC_00_Grapher_vert_hor.R"))
source(here("Code", "Functions", "FUNC_00_Fitness_Functions.R"))
source(here("Code", "Functions","FUNC_00_best_long_strategyfinder.R"))
source(here("Code", "Functions","PLOTTER_03_SurfacePlot_Maker.R"))

######################################################
### Makes Figure 2, which is the surface plot of the##
### acute phase duration and the fitness
####################################################
Fitness_MODEL_PC <- read.csv(here(
  "Output", "Fitness_Model",
  "FITNESS_MODEL_PCdeathYes.csv"
))
Fitness_MODEL_PF <- read.csv(here(
  "Output", "Fitness_Model",
  "FITNESS_MODEL_PFdeathYes.csv"
))

PC_GG_SurfacePlot <- MAIN_SURFACEPLOT_GG_GRAPHER_FIT(Fitness_MODEL_PC,"PC")
PF_GG_SurfacePlot <- MAIN_SURFACEPLOT_GG_GRAPHER_FIT(Fitness_MODEL_PF ,"PF")

PC_GG_SurfacePlot  + PF_GG_SurfacePlot 
###IF you want to see the optimal strategy (B_V/C_V)
Best_Strategy_Finder(Fitness_MODEL_PC);#15.5, 0.76
Best_Strategy_Finder(Fitness_MODEL_PF) #17 0.48

### 
Fitness_MODEL_PC_CV_OPT <- subset(Fitness_MODEL_PC, Fitness_MODEL_PC$C_V == 0.76)
Fitness_MODEL_PC_CV_OPT$species <- "PC"

Fitness_MODEL_PF_CV_OPT <- subset(Fitness_MODEL_PF, Fitness_MODEL_PF$C_V == 0.48)
Fitness_MODEL_PF_CV_OPT$species <- "PF"



Fitness_MODEL_CV_OPT <- rbind.data.frame(Fitness_MODEL_PC_CV_OPT , Fitness_MODEL_PF_CV_OPT )

ggplot(Fitness_MODEL_CV_OPT ,
       aes(x =B_V, y = endtime + 0.05, 
           fill =status))+
         geom_density(stat='identity',outline.type = 'full')+
         facet_wrap(~species)+
         scale_fill_manual(values = c('grey',"black",'#F14889'))+
         xlab("R")+
         ylab("Duration of acute phase")+
         theme_classic()+
         theme(strip.background = element_blank(),
               axis.text = element_text(size = 12),
               axis.title = element_text(size = 13),
               strip.text = element_text (size = 15))
  
  
ggplot(subset(Fitness_MODEL_CV_OPT ,
              Fitness_MODEL_CV_OPT$status == 'success'),
       aes(x = B_V, y = endtime + 0.05, 
           fill =species))+
  geom_density(stat='identity',outline.type = 'full', alpha = 0.5)+
  scale_fill_manual(name = "",values = c('#4B878BFF','#D01C1FFF'))+
  xlab("Burst size")+
  ylab("Duration of acute phase")+
  theme_classic()+
  theme(strip.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text = element_text (size = 15))



