Single_BV_CV_Simulator <- function(B_V, C_V, criss_cross, species,initial, variable_interest){
  trans_prob <- switch(species,
                       "PC" = PrI_PC,
                       "PF" = PrI_PF) 
    
  if(criss_cross == "NO"){
    
    sim_model <- switch(species,
                        "PC" =Simulator_Malaria_BC_PC ,
                        "PF" =Simulator_Malaria_BC_PF)
    
    ### Simulate with the cut-data
    model_sim_cut <- switch(species,
                            "PC" = Simulator_MalariaPC_DDE_BC_Cut,
                            "PF" = Simulator_MalariaPF_DDE_BC_Cut)
    

  full_model <- sim_model (B_V, C_V, initial, "Yes")
  duration_initial <- Finder_RM(full_model, species, "NO", NA)
  fitness <- model_sim_cut(B_V, C_V, initial, duration_initial$endtime)

  full_model$end_fitness <- Gametocyte_Fitness(fitness,species)
 
  fitness_2 <- data.frame(fitness)
  fitness_2$G[(fitness_2$G) < 0] <- 0
  full_model$prob <-(trans_prob(fitness_2$G))
  full_model$infection_length<- duration_initial $endtime
  
  }else if (criss_cross == "YES"){
    
    sim_model_cc <- switch(species,
                        "PC" = Simulator_PC_Criss_Cross  ,
                        "PF" = Simulator_PF_Criss_Cross )
    
    ### Simulate with the cut-data
    model_sim_cut_cc <- switch(species,
                            "PC" = Simulator_PC_Criss_Cross_Cut ,
                            "PF" = Simulator_PF_Criss_Cross_Cut 
    )
    
    
    
    full_model_cut <- sim_model_cc(variable_interest, B_V, NA, NA, C_V, initial, "Yes")
    duration_initial <- Finder_RM(full_model_cut , species, "YES",variable_interest )
    fitness <- model_sim_cut_cc(variable_interest,B_V, NA,
                             NA, C_V, initial, duration_initial $endtime)
    
    fitness_2 <- data.frame(fitness)
    fitness_2$G[(fitness_2$G) <0] <- 0
    full_model_cut$prob <-(trans_prob(fitness_2$G))
    
    full_model_cut$duration_initial<- duration_initial $endtime
    
    full_model<- full_model_cut 
  
    
    
  }
  return(full_model)
  
}
