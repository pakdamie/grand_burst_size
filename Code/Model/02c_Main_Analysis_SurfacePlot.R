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
  "FITNESS_MODEL_PC.csv"
))
Fitness_MODEL_PF <- read.csv(here(
  "Output", "Fitness_Model",
  "FITNESS_MODEL_PF.csv"
))



PC_GG_SurfacePlot <- MAIN_SURFACEPLOT_GG_GRAPHER_FIT(Fitness_MODEL_PC,"PC")
PF_GG_SurfacePlot <- MAIN_SURFACEPLOT_GG_GRAPHER_FIT(Fitness_MODEL_PF ,"PF")


###IF you want to see the optimal strategy (B_V/C_V)
Best_Strategy_Finder(Fitness_MODEL_PC);#15.5, 0.76
Best_Strategy_Finder(Fitness_MODEL_PF) #4.5 0.56


