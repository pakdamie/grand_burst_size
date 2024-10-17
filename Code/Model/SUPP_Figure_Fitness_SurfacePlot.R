# Script Name: 02_Main_Analysis_SurfacePlot.R
#
# Script Description: This is the script that takes the output
#from the previous analysis and produce the first surface plot 
#
# Notes: Originally, had the fitness and duration plot together,
# but changed it so that only the fitness is in main and the
# duration is in the supp.
#

library(here)

source(here("Code", "Functions", "FUNC_M_Sourcing_Plot_SurfacePlots.R"))

Fitness_MODEL_PC <- read.csv(here(
  "Output", "Fitness_Model",
  "FITNESS_MODEL_PC_NA.csv"
))
Fitness_MODEL_PF <- read.csv(here(
  "Output", "Fitness_Model",
  "FITNESS_MODEL_PF_NA.csv"
))

PC_GG_SurfacePlot <- MAIN_SURFACEPLOT_GG_GRAPHER_FIT(Fitness_MODEL_PC,"PC")
PF_GG_SurfacePlot <- MAIN_SURFACEPLOT_GG_GRAPHER_FIT(Fitness_MODEL_PF ,"PF")

PC_GG_SurfacePlot  + PF_GG_SurfacePlot 

###IF you want to see the optimal strategy (B_V/C_V)
Best_Strategy_Finder(Fitness_MODEL_PC);#15.5, 0.76
Best_Strategy_Finder(Fitness_MODEL_PF) #17 0.48

