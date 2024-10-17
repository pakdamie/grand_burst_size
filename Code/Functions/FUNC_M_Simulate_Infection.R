#' Simulate the infection for either Plasmodium chabaudi or Plasmodium
#' falciparum
#'
#'This is the main function for simulating infections that may either
#'fail to establish, establish and kills the host (if user includes death),
#'or successfully establishes and ends at the end of the acute phase. Because
#'the parameters are different for P. chabaudi or P. falciparum, you have to
#'specify which species you are interested in.
#'
#'
#' @param species "PC" or "PF" for P. chabaudi or P. falciparum
#' @param initial_value the initial value for inoculum (best to keep it default)
#' @param C_V_specific the transmission investment of investment 
#' @param variable_interest what parameter are you interested in varying
#' @param include_death can host die (best to keep it default)
#'
#' @return A data.frame that gives you the end fitness across all strains
#' (varying burst size and transmission investment combination)
#' @export
#'
#' @examples Simulate_Infection(species = "PC", initial_value = "default",
#' C_V_specific = NA, variable_interest = "pmax", include_death ="Yes")

Simulate_Infection <- function(
  species, 
  initial_value = "default",
  C_V_specific = NA, 
  variable_interest = NA,
  include_death = "Yes") {
  
  if (!(species %in% c("PC", "PF"))) {
    stop("Invalid input, either input `PC` for P. chabaudi or `PF`
    for P.falciparum")
  }

  if (!(variable_interest %in% c("pmax", "R_Modifier", "mu_M", "alpha1","combo"))) {
    stop("Invalid input for variable_interest, either input 'pmax', `R_Modifier`,
         `mu_M`, `combo`, or 'alpha1")
  }

  ### The first part of the model trying to figure out what
  ### parameter combinations we want to simulate

  ### Burst Size.
  B_V <- seq(1, 100, 0.5)

  ### If no value is given to C_V_specific, then use the default
  ### values.
  if (is.na(C_V_specific) == TRUE) {
    C_V <- seq(0.01, 1, 0.01) # Transmission investment
  } else {
    C_V <- C_V_specific
  }

  ### If by chance you want to change the initial parasite number-
  ### but use the default value if you have no reason to change it.
  if (initial_value == "default") {
    initial_value <- switch(
      species,
      "PC" = 4385.965,
      "PF" = 25000
    )
  } else {
    initial_value <- initial_value
  }

  ### Default modifiers of the variable of interest (multiplies the original
  ### value).
  p_val <- 1 # Invasion rate
  mu_M <- 1 # Merozoite mortality rate
  alpha1 <- 1 # Development Rate
  R_Modifier <- 1 # initial RBC


  ###If the variable of interest is not NA, then what should the variable
  ###of interest modifier be? I made it species specific due to weird
  ###things happening at the boundaries.
  
if (!is.na(variable_interest) && variable_interest == "R_Modifier") {
  R_Modifier <- switch(species,
    "PC" = c((1/3.7), (1/1.7), 1.0, 1.17, 6.57),
    "PF" = c((1/2.17), 1.0, (1.7) ,2.0, 11.18)
  )
#: PC: 8500000
#: PF: 5000000

} else if (!is.na(variable_interest) && variable_interest == "alpha1") {
  alpha1 <- switch(species,
    "PC" = c(1, 1/2, 1/3, 1/5, 1/6, 24, 48, 96),
    "PF" = c(1, 2, 2/3, 2/7, 4 )
  )
  
} else if (!is.na(variable_interest) && variable_interest == "combo") {
  p_val <- c(0.01, 0.25, 0.50, 1, 1.25, 1.50)
  mu_M <- switch(species,
    "PC" = c(1, 288/48, 200/48, 1440/48, 1/2, 1/48, (1/7)/48),
    "PF" = c(1, 288/200, 200, 48/200, 24/200, 1/200, (1/7)/200)
  )
}

  ### This is the main parameter
  ### values that you use to feed into the
  ### main simulation

  B_V_C_V <- expand.grid(
    B_V = B_V, #burst size
    C_V = C_V, #transmission investment
    p_val = p_val, #invasion rate
    mu_M = mu_M, #mu_M
    R_Modifier = R_Modifier, #Initial RBC 
    alpha_1 = alpha1, #asexual development time
    initialvalue = initial_value #initial RBC
  )

  ### We can already take out parameter combinations that would NOT
  ### lead to the establishment of infection by looking at the initial
  ### RM threshold

  Initial_RMs <- mcmapply(
    Calculate_Initial_RM,
    species = species,
    p_val = c(B_V_C_V$p_val),
    mu_M = c(B_V_C_V$mu_M),
    R_modifier = c(B_V_C_V$R_Modifier),
    alpha_1 = c(B_V_C_V$alpha_1),
    mc.cores = 3,
    SIMPLIFY = FALSE
  )


  ### Bind it to the data.frame
  B_V_C_V$Initial_RM <- do.call(rbind, Initial_RMs)

  ### If the replicative capacity (1-CV) B is greater than the RM threshold,
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
    alpha_1 = c(B_V_C_V_F$alpha_1),
    initialvalue = c(B_V_C_V_F$initialvalue),
    R_Modifier = c(B_V_C_V_F$R_Modifier),
    include_death = c(include_death),
    mc.cores = 3,
    SIMPLIFY = FALSE
  )

  ### Combine all list elements to make it easier to save/read
  FULL_MODEL_DT <- do.call(rbind, FULL_MODEL)

  ### Write into a CSV TO BE SAVED
  write.csv(FULL_MODEL_DT, 
    file = here(
      "Output", "Full_Model",
      paste("FULL_MODEL_DT", "_", species, "_", 
      variable_interest, ".csv", sep = "")
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
  Duration_Initial$alpha_1 <- B_V_C_V_F$alpha_1
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

  ### This is now the full model simulation where we run it until
  ### the acute phase
  if (nrow(Duration_Initial_SUCCESS) != 0) {
    Fitness_MODEL <- mcmapply(
      model_sim_cut,
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
      unlist(lapply(Fitness_MODEL, 
                    Gametocyte_Fitness, 
                    species = species))
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
