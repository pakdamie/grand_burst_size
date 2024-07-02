### This is the function for simulating the infections
### species = "PC" (chabaudi) or "PF" (falciparum)
### initial value = initial inoculum,
### C_V_specific = If NA, that means look at across 1% to 100%. If a value is
### given, run it just for that.
### The variable of interest should be either "mu_M' (mortality rate of merozoite),
### R_Modifier (initial RBCs), 'pmax' (invasion rate), and 'alpha1',
### these are multiplicative
### modifiers
Simulate_Infection <- function(species, initial_value = "default",
                               C_V_specific = NA,
                               variable_interest = NA,
                               include_death = "Yes") {
  if (!(species %in% c("PC", "PF"))) {
    stop("Invalid input, either input `PC` for P. chabaudi or `PF`
    for P.falciparum")
  }

  if (!(variable_interest %in% c("pmax", "R_Modifier", "mu_M", "alpha1"))) {
    stop("Invalid input for variable_interest, either input 'pmax', `R_Modifier`,
         `mu_M`, or 'alpha1")
  }

  ### The first part of the model trying to figure out what
  ### parameter combinations we want to simulate

  ### Burst Size and Transmission Investment ###
  B_V <- seq(1, 100, 1) # Burst size

  if (is.na(C_V_specific) == TRUE) {
    C_V <- seq(0.01, 1, 0.01) # Transmission investment
  } else {
    C_V <- C_V_specific
  }

  ### If the variable of interest is "pmax'
  if (is.na(variable_interest) == FALSE & variable_interest == "pmax") {
    ### Invasion rate
    p_val <- c(c(1e-5,1e-4,1e-3,0.01,0.05,0.10,0.25,0.50,0.75,1),seq(2,4,length = 10),seq(5,100,5))
    alpha1 <- 1
    mu_M <- 1
    R_Modifier <- 1
    
  } else if (is.na(variable_interest) == FALSE & variable_interest == "mu_M") {
    mu_M <- c(c(1e-5,1e-4,1e-3, 0.01,0.05,0.10,0.25,0.50,0.75,1),seq(2,4,length = 5),seq(5,100,5))
    p_val <- 1
    R_Modifier <- 1
    alpha1 <- 1
  } else if (is.na(variable_interest) == FALSE & variable_interest == "R_Modifier") {
    R_Modifier <- c(c(1e-5,1e-4,1e-3, 0.01,0.05,0.10,0.25,0.50,0.75,1),seq(2,4,length = 5),seq(5,100,5))
    p_val <- 1
    mu_M <- 1
    alpha1 <- 1
  } else if (is.na(variable_interest) == FALSE & variable_interest == "alpha1") {
    alpha1 <- c(c(0.25,0.50,0.75,1),seq(2,5,length = 20))
    p_val <- 1
    mu_M <- 1 
    R_Modifier <- 1
  } else if (is.na(variable_interest) == TRUE) {
    p_val <- 1
    mu_M <- 1
    R_Modifier <- 1
    alpha1 <- 1
  }

  if (initial_value == "default") {
    initial_value <- switch(species,
      "PC" = 4385.965,
      "PF" = 25000
    )
  } else {
    initial_value <- initial_value
  }

  B_V_C_V <- expand.grid(
    B_V = B_V,
    C_V = C_V,
    p_val = p_val,
    mu_M = mu_M,
    R_Modifier = R_Modifier,
    alpha_1 = alpha1,
    initialvalue = initial_value
  )

  ### We can already take out parameter combinations that would not
  ### lead to the establishment of infection by looking at the initial
  ### RM threshold

  Initial_RMs <- mcmapply(Calculate_Initial_RM,
    species = species,
    p_val = c(B_V_C_V$p_val),
    mu_M = c(B_V_C_V$mu_M),
    R_modifier = c(B_V_C_V$R_Modifier),
    alpha_1 = c(B_V_C_V$alpha_1),
    mc.cores = 3,
    SIMPLIFY = FALSE
  )

  B_V_C_V$Initial_RM <- do.call(rbind, Initial_RMs)


  ### If the replicative capacity (1-CV)B is greater than the RM threshold,
  ### then the infection is established.

  B_V_C_V$Establish <- ifelse((1 - B_V_C_V$C_V) * B_V_C_V$B_V >= B_V_C_V$Initial_RM,
    "Establish",
    "Fail"
  )

  ### Subset parameter estimations that are successful (may kill host!)
  B_V_C_V_F <- subset(B_V_C_V, B_V_C_V$Establish == "Establish")

  ### We then use the B_V_C_V_F as the variable list to run our model.

  ### Using our simulator specific to the species
  model_sim <- switch(species,
    "PC" = Simulator_Malaria_BC_PC,
    "PF" = Simulator_Malaria_BC_PF
  )
  
  ### Run the Full Model
  FULL_MODEL <- mcmapply(model_sim,
    B_V = c(B_V_C_V_F$B_V),
    C_V = c(B_V_C_V_F$C_V),
    p_val = c(B_V_C_V_F$p_val),
    mu_M = c(B_V_C_V_F$mu_M),
    alpha_1 =c(B_V_C_V_F$alpha_1),
    initialvalue = c(B_V_C_V_F$initialvalue),
    R_Modifier = c(B_V_C_V_F$R_Modifier),
    include_death = c(include_death),
    mc.cores = 3,
    SIMPLIFY = FALSE
  )

  ### Combine all list elements to make it easier to save/read
  FULL_MODEL_DT <- do.call(rbind, FULL_MODEL)

  ### Write into a CSV TO BE SAVED
  write.csv(FULL_MODEL_DT, file = here(
    "Output", "Full_Model",
    paste("FULL_MODEL_DT", "_", species, "_", variable_interest, ".csv", sep = "")
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
  Duration_Initial$alpha_1<- B_V_C_V_F$alpha_1
  Duration_Initial$R_Modifier <- B_V_C_V_F$R_Modifier


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
      mu_M = Failed_B_V_C_V$mu_M,
      alpha_1 = Failed_B_V_C_V$alpha_1,
      R_Modifier = Failed_B_V_C_V$R_Modifier
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

  
  ###This is now the full model simulation where we run it until
  ### the acute phase
  if (nrow(Duration_Initial_SUCCESS) != 0) {
    Fitness_MODEL <- mcmapply(model_sim_cut,
      B_V = c(Duration_Initial_SUCCESS$B_V),
      C_V = c(Duration_Initial_SUCCESS$C_V),
      p_val = c(Duration_Initial_SUCCESS$p_val),
      mu_M = c(Duration_Initial_SUCCESS$mu_M),
      alpha_1 = c(Duration_Initial_SUCCESS$alpha_1),
      R_Modifier = c(Duration_Initial_SUCCESS$R_Modifier),
      initialvalue = c(initial_value),
      endtime = c(Duration_Initial_SUCCESS$endtime),
      mc.cores = 3,
      SIMPLIFY = FALSE
    )

    Duration_Initial_SUCCESS$end_fitness <-
      unlist(lapply(Fitness_MODEL, Gametocyte_Fitness, species = species))
  } else {
    Duration_Initial_SUCCESS <- NA
  }
  ### These are the fitness model data.frame that should work
  Fitness_MODEL_FULL <- rbind.data.frame(
    Duration_Initial_SUCCESS,
    Duration_Initial_FAIL,
    Duration_Initial_MORT
  )

  ### Write into a CSV TO BE SAVED
  write.csv(Fitness_MODEL_FULL, file = here(
    "Output", "Fitness_Model",
    paste("FITNESS_MODEL_", species, "_", variable_interest, ".csv", sep = "")
  ))

  return(Fitness_MODEL_FULL)
}
