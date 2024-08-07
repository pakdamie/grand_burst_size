Rescale_Rate_Variable_Interest <- function(PC_fit_dat,PF_fit_dat,variable_interest){
  
  Fitness_PC <- Find_OptimalBV_Group(PC_fit_dat, variable_interest)
  Fitness_PF <- Find_OptimalBV_Group(PF_fit_dat, variable_interest)
  
  if (variable_interest == 'mu_M'){
    Fitness_PC$mu_M_rescale <- (48 * Fitness_PC$mu_M)
    Fitness_PF$mu_M_rescale <- (200 *Fitness_PF$mu_M)
  }

  if (variable_interest == 'p_val'){
    Fitness_PC$p_val_rescale <- (4.0e-6 * Fitness_PC$p_val)
    Fitness_PF$p_val_rescale <- (8.35e-6 * Fitness_PF$p_val)
  }
  
  if (variable_interest == 'R_Modifier'){
    Fitness_PC$R_Modifier_rescale <- (8500000* Fitness_PC$R_Modifier)
    Fitness_PF$R_Modifier_rescale <- (5e6* Fitness_PF$R_Modifier)
  }
  if (variable_interest == 'alpha_1'){
    Fitness_PC$alpha_1_rescale <- (1/(1 * Fitness_PC$alpha_1))
    Fitness_PF$alpha_1_rescale <-( 1/((1/2) * Fitness_PF$alpha_1) )
    }
  
  
  
  Fitness_PC$controlornot <- ifelse(Fitness_PC[,variable_interest] == 1, 
                                    'control','not')
  
  
  Fitness_PF$controlornot <- ifelse(Fitness_PF[,variable_interest] == 1, 
                                    'control','not')
  
  

Fitness_PC$species <- "P.chabaudi"
Fitness_PF$species <- "P.falciparum"
Fitness_All <- rbind(Fitness_PC ,Fitness_PF)
return(Fitness_All)
}

