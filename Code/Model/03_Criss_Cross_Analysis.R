#The parasite traits that we're interested in'

source(here("Code","Model","simulator_code","Simulator_MAIN_2_PC_CRISSCROSS.R"))
source(here("Code","Model","simulator_code","Simulator_MAIN_2_PF_CRISSCROSS.R"))
sourceCpp(here("Code", "Model", "rcpp", "rcpp_malaria_dynamics_CUT.cpp"))
sourceCpp(here("Code", "Model","rcpp", "rcpp_malaria_dynamics_UNCUT.cpp"))
source(here("Code", "Functions", "FUNC_00_Fitness_Functions.R"))
source(here("Code", "Functions", "FUNC_MODEL_Simulator_Code.R"))
source(here("Code","Functions","Optimal_Value_Interest_Finder.R"))



variable_interest <- c('pmax','alpha1', 'alpha2','muM','muG', 'cv')


initial_value_PC <- 4385.965
C_V_PC <- 0.76
C_V_PF <- 0.56
id = NA
species_PC = "PC"

PC_Interest_Params <- data.frame(variable_interest = variable_interest,
                                  initial_value = initial_value_PC,
                                  C_V_opt = NA,
                                  C_V_PC = 0.76,
                                  C_V_PF = 0.56,
                                  id = id,
                                  species = species_PC)


FULL_MODEL_VALUE_INTEREST_PC <-  mcmapply(FULL_MODEL_SIMULATING_Duration_criss_cross,
                                       c(PC_Interest_Params$variable_interest),
                                       c(PC_Interest_Params$initial_value),
                                       c(NA),
                                       c(PC_Interest_Params$C_V_PC),
                                       c(PC_Interest_Params$C_V_PF),
                                       c(PC_Interest_Params $id),
                                       c(PC_Interest_Params $species),
                                       mc.cores = 3,
                                       SIMPLIFY = FALSE)


FULL_MODEL_SIMULATING_Duration_criss_cross("cv",4385.965, NA,
                                           0.54, 0.54, NA, "PC")


FULL_MODEL_VALUE_INTEREST_PC_DT <- do.call(rbind,FULL_MODEL_VALUE_INTEREST_PC)


write.csv(FULL_MODEL_VALUE_INTEREST_PC_DT, file = "FULL_MODEL_VALUE_INTEREST_PC_DT.csv")

###
value_interest <- c('pmax','alpha1', 'alpha2','muM','muG')
initial_value_PF <- 25000
C_V_opt <- 0.76
id = NA
species_PF = "PF"

PF_Interest_Params <- expand.grid(value_interest =
                                    value_interest,
                                    initial_value =initial_value_PF,
                                    C_V_opt = C_V_opt,
                                    id = id,
                                    species = species_PF)



FULL_MODEL_VALUE_INTEREST_PF <-  mcmapply(FULL_MODEL_SIMULATING_Duration_criss_cross,
                                          c(PF_Interest_Params $value_interest),
                                          c(PF_Interest_Params $initial_value),
                                          c(PF_Interest_Params$C_V_opt),
                                          c(PF_Interest_Params $id),
                                          c(PF_Interest_Params $species),
                                          mc.cores = 3,
                                          SIMPLIFY = FALSE)
FULL_MODEL_VALUE_INTEREST_PF_DT <- do.call(rbind,FULL_MODEL_VALUE_INTEREST_PF)


write.csv(FULL_MODEL_VALUE_INTEREST_PF_DT, file = "FULL_MODEL_VALUE_INTEREST_PF_DT.csv")


PC_OPT <- Optimal_burst_size_finder(as.data.frame(FULL_MODEL_VALUE_INTEREST_PC_DT), "PC")
PF_OPT <- Optimal_burst_size_finder(FULL_MODEL_VALUE_INTEREST_PF_DT,"PF")
                          
ALL_OPT <- rbind(PC_OPT, PF_OPT)

write.csv(ALL_OPT ,here("Output","ALL_OPT.csv"))

          