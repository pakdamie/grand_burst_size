





FULL_MODEL_SIMULATING_Duration <- function(initial_value, species, include_death) {
  
  ### THE FIRST PART OF THE MODEL, trying to figure out what
  ### parameter combinations we want to simulate
  
  ### Burst Size and Transmission Investment ###
  B_V <- seq(1, 50, 0.5) # Burst size
  C_V <- seq(0.01, 1, 0.01) # Transmission investment
  
  # Different combinations of burst size, transmission investment, as
  # well as initial value
  
  B_V_C_V <- expand.grid(
    B_V = B_V,
    C_V = C_V,
    initialvalue = initial_value
  )
  
  ### We can already take out parameter combinations that would not
  ### lead to the establishment of infection by looking at the initial
  ### RM threshold
  
  RM_limit_1 <- Calculate_Initial_RM(species)
  
  ### If the replicative capacity (1-CV)B is greater than the RM threshold,
  ### then the infection is established.
  
  B_V_C_V$Establish <- ifelse((1 - B_V_C_V$C_V) * B_V_C_V$B_V >= RM_limit_1,
                              "Establish",
                              "Fail"
  )
  ### Subset parameter estimations that are successful (may kill host!)
  B_V_C_V_F <- subset(B_V_C_V, B_V_C_V$Establish == "Establish")
  
  ### Using our simulator specific to the species
  model_sim <- switch(species,
                      "PC" = Simulator_Malaria_BC_PC,
                      "PF" = Simulator_Malaria_BC_PF
  )
  
  ### Run the Full Model###
  
  FULL_MODEL <- mcmapply(model_sim,
                         c(B_V_C_V_F$B_V),
                         c(B_V_C_V_F$C_V),
                         c(B_V_C_V_F$initialvalue),
                         c(include_death),
                         mc.cores = 3,
                         SIMPLIFY = FALSE
  )
  
  ### Combine all list elements to make it easier to save/read
  FULL_MODEL_DT <- do.call(rbind, FULL_MODEL)
  
  ### Write into a CSV TO BE SAVED
  write.csv(FULL_MODEL_DT, file = here(
    "Output", "Full_Model",
    paste("FULL_MODEL_DT", species,"death", include_death, ".csv", sep = "")
  ))
  
  ### Remove it now to save space
  remove(FULL_MODEL_DT)
  
  
  #########################################################
  ### THIS THEN FIGURES OUT THE DURATION OF THE ACUTE PHASE#
  #########################################################
  
  Duration_Initial <-
    do.call(
      rbind,
      mclapply(FULL_MODEL,
               Finder_RM,
               species = species,
               criss_cross = "NO",
               mc.cores = 2
      )
    )
  
  Duration_Initial$B_V <- B_V_C_V_F$B_V
  Duration_Initial$C_V <- B_V_C_V_F$C_V
  
  
  ### Now we can find the parameter combinations that lead to
  ### unestablished infection- we then set the values
  ### manually to 0
  
  Failed_B_V_C_V <- subset(B_V_C_V, B_V_C_V$Establish == "Fail")
  
  Duration_Initial_FAIL <-
    data.frame(
      endtime = 0,
      up_down = 0,
      end_fitness = 0,
      status = "Fail",
      B_V = Failed_B_V_C_V$B_V,
      C_V = Failed_B_V_C_V$C_V
    )
  
  ### These are the B_V/C_V that would lead to mortality,
  ### This means that we know the end time (point of death) and the
  ### cumulative transmission potential
  Duration_Initial_MORT <- subset(
    Duration_Initial,
    Duration_Initial$status != "success"
  )
  
  Duration_Initial_SUCCESS <- subset(
    Duration_Initial,
    Duration_Initial$status == "success"
  )
  
  
  ### Simulate with the cut-data
  model_sim <- switch(species,
                      "PC" = Simulator_MalariaPC_DDE_BC_Cut,
                      "PF" = Simulator_MalariaPF_DDE_BC_Cut
  )
  
  
  Fitness_MODEL <- mcmapply(model_sim,
                            c(Duration_Initial_SUCCESS$B_V),
                            c(Duration_Initial_SUCCESS$C_V),
                            c(initial_value),
                            c(Duration_Initial_SUCCESS$endtime),
                            mc.cores = 5,
                            SIMPLIFY = FALSE
  )
  
  Duration_Initial_SUCCESS$end_fitness <-
    unlist(lapply(Fitness_MODEL, Gametocyte_Fitness, species = species))
  
  ### These are the fitness model data.frame that should work
  Fitness_MODEL_FULL <- rbind.data.frame(
    Duration_Initial_SUCCESS,
    Duration_Initial_FAIL,
    Duration_Initial_MORT
  )
  
  ### Write into a CSV TO BE SAVED
  write.csv(Fitness_MODEL_FULL, file = here(
    "Output", "Fitness_Model",
    paste("FITNESS_MODEL_", species, "death",include_death, ".csv", sep = "")
    
  ))
  
  
  return(Fitness_MODEL_FULL)
}
