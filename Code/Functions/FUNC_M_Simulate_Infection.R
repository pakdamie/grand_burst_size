### This is the function for simulating the infections
### species = "PC" (chabaudi) or "PF" (falciparum)
### initial value = initial inoculum,
### C_V_specific = If NA, that means look at across 1% to 100%. If a value is 
### given, run it just for that.

Simulate_Infection <- function(species, initial_value = "default",
                                           C_V_specific = NA,
                                           variable_interest = NA, 
                                           include_death = "Yes") {
  
  if (!(species %in% c("PC", "PF"))) {
    stop("Invalid input, either input `PC` for P. chabaudi or `PF`
    for P.falciparum")
  }


  ### The first part of the model trying to figure out what
  ### parameter combinations we want to simulate
  
  ### Burst Size and Transmission Investment ###
  B_V <- seq(1, 50, 0.5) # Burst size

  if (is.na(C_V_specific) == TRUE) {
    C_V <- seq(0.01, 1, 0.01) # Transmission investment
  } else {
    C_V <- C_V_specific
  }

  if (is.na(variable_interest) == FALSE & variable_interest == "pmax") {
    ### Invasion rate
    p_val <- switch(species,
      "PC" = seq(1.0e-7, 1.74e-05, length = 50),
      "PF" = seq(1.0e-7,  1.74e-05, length = 50)
    )
    mu_M <- switch(species,
      "PC" = 48,
      "PF" = 200
    )
  } else if (is.na(variable_interest) == FALSE & variable_interest == "mu_M") {
    mu_M <- seq(1/24, 600, length = 100)

        p_val <- switch(species,
      "PC" = 4.0e-6,
      "PF" = 8.35e-6
    )
  } else if (is.na(variable_interest) == TRUE) {
    p_val <- switch(species,
      "PC" = 4.0e-6,
      "PF" = 8.35e-6
    )
    ### Merozoite mortality rate
    mu_M <- switch(species,
      "PC" = 48,
      "PF" = 200
    )
  }
  
  if (initial_value == "default"){
    initial_value <- switch(species,
                   "PC" = 4385.965,
                   "PF" = 25000
    )
  }else{
    initial_value <- initial_value
  }
  
  
  
  B_V_C_V <- expand.grid(
    B_V = B_V,
    C_V = C_V,
    p_val = p_val,
    mu_M = mu_M ,
    initialvalue = initial_value)
  
  ### We can already take out parameter combinations that would not
  ### lead to the establishment of infection by looking at the initial
  ### RM threshold
  
  Initial_RMs <- mcmapply(Calculate_Initial_RM,
                         species,
                         c(B_V_C_V$p_val),
                         c(B_V_C_V$mu_M),
                         mc.cores = 3,
                         SIMPLIFY = FALSE)
  
  B_V_C_V$Initial_RM <- do.call(rbind,Initial_RMs)
  
  
  ### If the replicative capacity (1-CV)B is greater than the RM threshold,
  ### then the infection is established.
  
  B_V_C_V$Establish <- ifelse((1 - B_V_C_V$C_V) * B_V_C_V$B_V >= B_V_C_V$Initial_RM ,
                              "Establish",
                              "Fail"
  )
  ### Subset parameter estimations that are successful (may kill host!)
  B_V_C_V_F <- subset(B_V_C_V, B_V_C_V$Establish == "Establish")
  
  ###We then use the B_V_C_V_F as the variable list to run our model.
  
  ### Using our simulator specific to the species
  model_sim <- switch(species,
                      "PC" = Simulator_Malaria_BC_PC,
                      "PF" = Simulator_Malaria_BC_PF
  )
  
  ### Run the Full Model
  FULL_MODEL <- mcmapply(model_sim,
                         c(B_V_C_V_F$B_V),
                         c(B_V_C_V_F$C_V),
                         c(B_V_C_V_F$p_val),
                         c(B_V_C_V_F$mu_M),
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
    paste("FULL_MODEL_DT","_", species,"_",variable_interest,".csv", sep = "")
  ))
  
  ### Remove it now to save space
  remove(FULL_MODEL_DT)
  
  ### Part 2: 
  ### THIS THEN calculates OUT THE DURATION OF THE ACUTE PHASE
  
  Duration_Initial <-
    do.call(
      rbind,
      mclapply(FULL_MODEL,
               Finder_RM,
               species = species,
               mc.cores = 2
      )
    )
  
  Duration_Initial$B_V <- B_V_C_V_F$B_V
  Duration_Initial$C_V <- B_V_C_V_F$C_V
  Duration_Initial$p_val <- B_V_C_V_F$p_val
  Duration_Initial$mu_M <- B_V_C_V_F$mu_M
  
  
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
      C_V = Failed_B_V_C_V$C_V,
      p_val = Failed_B_V_C_V$p_val,
      mu_M = Failed_B_V_C_V$mu_M
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
  model_sim_cut <- switch(species,
                      "PC" = Simulator_MalariaPC_DDE_BC_Cut,
                      "PF" = Simulator_MalariaPF_DDE_BC_Cut
  )
  
  
  Fitness_MODEL <- mcmapply(model_sim_cut ,
                            c(Duration_Initial_SUCCESS$B_V),
                            c(Duration_Initial_SUCCESS$C_V),
                            c(Duration_Initial_SUCCESS$p_val),
                            c(Duration_Initial_SUCCESS$mu_M),
                            c(initial_value),
                            c(Duration_Initial_SUCCESS$endtime),
                            mc.cores =3,
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
    paste("FITNESS_MODEL_", species,"_",variable_interest, ".csv", sep = "")
    
  ))
  
  return(Fitness_MODEL_FULL)
}
