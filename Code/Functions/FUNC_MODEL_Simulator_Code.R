###########################################################
### THIS IS THE FUNCTION THAT SIMULATES INFECTIONS         #
### AND TELLS YOU THE FITNESS AT THE END OF THE ACUTE PHASE#
###########################################################

### This calculates the initial RM that allows for the parasites
### to establish. Species include "PC" or "PF"
### indicate that the parameters are switched.

Calculate_Initial_RM <- function(species) {
  
  initial_RM_modifier <- 1.5 # The threshold for establishment.
  # If we're not criss-crossing the parameter values

  time_delayer <- switch(species,
    "PC" = ((100 * (1 / 1) + (1 / 40))^100) / (100 * (1 / 1))^100,
    "PF" = ((10 * (1 / 2) + (1 / 120))^10) / (10 * (1 / 2))^10
  )

  p_val <- switch(species, # INVASION RATE
    "PC" = 4.0e-6,
    "PF" = 8.35e-6
  )

  mu_M <- switch(species, # MEROZOITE MORTALITY
    "PC" = 48,
    "PF" = 200
  )

  R_val <- switch(species, # INITIAL RBC
    "PC" = 8500000,
    "PF" = 5e6
  )

  RM_limit_1 <- initial_RM_modifier * time_delayer * ((p_val * R_val) + mu_M) / (p_val * R_val)

  return(RM_limit_1)
}

### This calculates the initial RM for when you're criss-crossing
### each individual parameter

Calculate_Initial_RM_CrissCross <- function(species, variable_interest) {
  initial_RM_modifier <- 1.5 # The threshold for establishment.

  if (species == "PC") { # If the species is Plasmodium chabaudi
    R_val <- 8500000
    pmax_val <- ifelse(variable_interest != "pmax", 4.0e-6, 8.35e-6)
    alpha1_val <- ifelse(variable_interest != "alpha1", 1, 1 / 2)
    alpha2_val <- ifelse(variable_interest != "alpha2", 1 / 2, 1 / 7)
    muM_val <- ifelse(variable_interest != "muM", 48, 200)
    muG_val <- ifelse(variable_interest != "muG", 4, log(2) / 2.4)

    time_delayer_CROSSED <- ((100 * alpha1_val + (1 / 40))^100) / (100 * alpha1_val)^100

    RM_limit_1 <- initial_RM_modifier * time_delayer_CROSSED * ((pmax_val * R_val) + muM_val) / (pmax_val * R_val)
  } else if (species == "PF") {
    R_val <- 5e6
    pmax_val <- ifelse(variable_interest != "pmax", 8.35e-6, 4.0e-6)
    alpha1_val <- ifelse(variable_interest != "alpha1", 1 / 2, 1)
    alpha2_val <- ifelse(variable_interest != "alpha2", 1 / 7, 1 / 2)
    muM_val <- ifelse(variable_interest != "muM", 200, 48)
    muG_val <- ifelse(variable_interest != "muG", log(2) / 2.4, 4)


    time_delayer_CROSSED <- ((10 * alpha1_val + (1 / 120))^10) / (10 * alpha1_val)^10

    RM_limit_1 <- initial_RM_modifier * time_delayer_CROSSED * ((pmax_val * R_val) + muM_val) / (pmax_val * R_val)
  } else {
    warning("You either need to write PC or PF for species")
  }
  return(RM_limit_1)
}


### SIMULATE THE MODEL AS IS FOR PLASMODIUM CHABAUDI OR PLASMODIUM
### FALCIPARUM

# Input:
# 1)initial_value - The different intial values for the infected RBC inoculum
# 2) mu_M_value - the mortality rate of merozoite, crucial for figuring out
# which infections are unestablished
# 3 id - give it a low, med, high id

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
    c("No"),
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


### SIMULATE The MODEL FOR CRISS_CROSSING FOR PLASMODIUM CHABAUDI OR
### PLASMODIUM FALCIPARUM
FULL_MODEL_SIMULATING_Duration_criss_cross <- function(variable_interest = 'cv',
                                                       initial_value = 4385.965,
                                                       C_V_opt = NA,
                                                       C_V_PC. =0.54,
                                                       C_V_PF = 0.54,
                                                       id = NA,
                                                       species = "PC") {
if (is.na(C_V_opt) == TRUE) {
    if (variable_interest != "cv") {
      C_V_val <- switch(species,
        "PC" =  C_V_PC,
        "PF" =  C_V_PF
      )
    } else if (variable_interest == "cv") {
      C_V_val <- switch(species,
        "PC" =  C_V_PC,
        "PF" =  C_V_PF
      )
    }
else{
    C_V_val = C_V_opt
}
}

  R_0 <- seq(1, 50, 0.5) # BV

  if (variable_interest == "alpha1") {
    R_0_PC <- R_0^2 # For chabaudi (We're criss-crossing)
    R_0_PF <- R_0
  } else if (variable_interest != "alpha1") {
    R_0_PC <- R_0 # For chabaudi
    R_0_PF <- R_0^2
  } # For falciparum


  ### THE FIRST PART OF THE MODEL, trying to figure out what
  ### parameter combinations we want to be looking at
  ### Burst Size and Transmission Investment ###

  B_V <- switch(as.character(species),
    "PC"  =   R_0_PC,
    "PF" =    R_0_PF
  )
  # Different combinations
  B_V_C_V <- expand.grid(
    B_V = B_V,
    C_V = C_V_val,
    initialvalue = initial_value
  )


  ### We can already take out parameter combinations that would not
  ### lead to the establishment of infection

  RM_limit_1 <- Calculate_Initial_RM_CrissCross(species, variable_interest)




  B_V_C_V$Establish <- ifelse((1 - B_V_C_V$C_V) * B_V_C_V$B_V >= RM_limit_1,
    "Establish", "Fail"
  )

  ### Simulate infections that are successful (may kill host!)
  B_V_C_V_F <- subset(B_V_C_V, B_V_C_V$Establish == "Establish")

  ### These infections are successful OR lead to host mortality

  model_sim <- switch(as.character(species),
    "PC" = Simulator_PC_Criss_Cross,
    "PF" = Simulator_PF_Criss_Cross
  )
  if (nrow(B_V_C_V_F) != 0) {
    
    FULL_MODEL <- mcmapply(model_sim,
      c(variable_interest),
      c(B_V_C_V_F$B_V),
      c(C_V_PC),
      c(C_V_PF),
      c(C_V_opt),
      c(B_V_C_V_F$initialvalue),
      mc.cores = 3,
      SIMPLIFY = FALSE
    )

    ### Combine all list elements to make it easier to save/read
    FULL_MODEL_DT <- do.call(rbind, FULL_MODEL)

    ### Write into a CSV TO BE SAVED
    write.csv(FULL_MODEL_DT, file = here(
      "Output", "Full_Model",
      paste("FULL_MODEL_DT", species, variable_interest, ".csv", sep = "")
    ))

    ### REMOVE right now to save space
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
          criss_cross = "YES",
          variable_interest = variable_interest,
          mc.cores = 2
        )
      )

    Duration_Initial$B_V <- B_V_C_V_F$B_V
    Duration_Initial$C_V <- B_V_C_V_F$C_V


    ### Now we can find the parameter combinations that lead to
    ### unestablished infection- we then set the values
    ### manually to 9

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

    model_sim_cut <- switch(as.character(species),
      "PC" = Simulator_PC_Criss_Cross_Cut,
      "PF" = Simulator_PF_Criss_Cross_Cut
    )

    print(Duration_Initial_SUCCESS$B_V)

    Fitness_MODEL <- mcmapply(model_sim_cut,
      c(variable_interest),
      c(Duration_Initial_SUCCESS$B_V),
      c(C_V_PC),
      c(C_V_PF),
      c(NA),
      c(initial_value),
      c(Duration_Initial_SUCCESS$endtime),
      mc.cores = 3,
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

    Fitness_MODEL_FULL$variable_interest <- variable_interest

    ### Write into a CSV TO BE SAVED
    write.csv(Fitness_MODEL_FULL, file = here(
      "Output", "Fitness_Model",
      paste("FITNESS_MODEL_", species, variable_interest, ".csv", sep = "")
    ))


    return(Fitness_MODEL_FULL)
  } else {
    return(NA)
  }
}
