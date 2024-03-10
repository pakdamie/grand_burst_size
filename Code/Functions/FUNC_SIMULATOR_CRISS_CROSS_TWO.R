
Calculate_Initial_RM_CrissCross_Two <- function(species, variable_interest_1, variable_interest_2) {
 
  variable_interest_vec <- c(variable_interest_1, variable_interest_2)
  
   initial_RM_modifier <- 1.5 # The threshold for establishment.
  
  if (species == "PC") { # If the species is Plasmodium chabaudi
    R_val <- 8500000
    pmax_val <- ifelse(!("pmax" %in% variable_interest_vec), 4.0e-6,8.35e-6)
    alpha1_val <- ifelse(!("alpha1" %in% variable_interest_vec), 1,  1 / 2)
    alpha2_val <- ifelse(!("alpha2" %in% variable_interest_vec),  1 / 2, 1/7)
    muM_val <- ifelse(!("muM" %in% variable_interest_vec),48, 200)
    muG_val <- ifelse(!("muG"%in% variable_interest_vec), 4, log(2) / 2.4)
    
    
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


RM_Calculator_Criss_Cross_Two <- function(species, x_list, variable_interest_1, variable_interest_2) {
 
  variable_interest_vec <- c(variable_interest_1, variable_interest_2)
  
  
   if (species == "PC") {
    
    pmax_val <- ifelse(!("pmax" %in% variable_interest_vec), 4.0e-6,8.35e-6)
    alpha1_val <- ifelse(!("alpha1" %in% variable_interest_vec), 1,  1 / 2)
    muM_val <- ifelse(!("muM" %in% variable_interest_vec),48, 200)

    muM_val <- ifelse(variable_interest != "muM", 48, 200)
    PC_Time_Delayer <- ((100 * (alpha1_val))^100) / ((100 * (alpha1_val) + 0.025))^100
    
    unique_B_V <- unique(x_list$B_V)
    unique_C_V <- unique(x_list$C_V)
    
    rate <- PC_Time_Delayer * (1 - unique_C_V) * unique_B_V *
      ((x_list[, "R"] * pmax_val) / ((pmax_val * x_list[, "R"]) + muM_val))
    
  } else if (species == "PF") {
    
    pmax_val <- ifelse(variable_interest != "pmax", 8.35e-6, 4.0e-6)
    alpha1_val <- ifelse(variable_interest != "alpha1", 1 / 2, 1)
    muM_val <- ifelse(variable_interest != "muM", 200, 48)

    PF_Time_Delayer <- ((10 * (alpha1_val))^10 )/ ((10 * (alpha1_val) + (1 / 120)))^10
    
    unique_B_V <- unique(x_list$B_V)
    unique_C_V <- unique(x_list$C_V)
    
    rate <- PF_Time_Delayer * (1 - unique_C_V) * unique_B_V *
      ((x_list[, "R"] * pmax_val) / ((pmax_val * x_list[, "R"]) + muM_val))
  }
  
  return(rate)
}


###################################################
### INPUT: The element of the full deSolve output###
### as well as the mu_M                         ###
###################################################
Finder_RM_Two <- function(x_list, species, variable_interest_1, variable_interest_2) {
  

  
  ### If it's NA that means that it never kills the host,
  ### If it's a numeric value, that means it kills the host.
  
  infection_length <- unique(x_list$infection_length)
  
  ### If the infection_length is NA.
  if (is.na(infection_length) == FALSE) {
    ### Look at the gametocyte time series
    ### Calculating the fitness
    G_TS <- data.frame(
      time = x_list[, "time"],
      G = x_list[, "G"]
    )
    
    # Prevent negative gametocytes if there are any
    G_TS$G[G_TS$G < 0] <- 0
    
    ### Let's approximate when the gametocyte time series hit
    ### The time...
    G_TS_Function <- approxfun(G_TS)
    
    truncate_G_TS <- G_TS_Function(seq(0, infection_length, 1 / 10))
    
    fitness_func <- switch(species,
                           "PC" = PrI_PC,
                           "PF" = PrI_PF
    )
    
    end_fitness_mort <- sum(fitness_func(truncate_G_TS) * 1 / 10)
    
    df <- data.frame(
      endtime = infection_length,
      up_down = "up",
      end_fitness = end_fitness_mort,
      status = "mort"
    )
  } else {
    
    unique_B_V <- unique(x_list$B_V)
    unique_C_V <- unique(x_list$C_V)
    
    
      RM_time_df <- cbind.data.frame(
        time = x_list[, "time"],
        rate = RM_Calculator_Criss_Cross_Two(species, x_list, variable_interest_1, variable_interest_2),
        B_V = unique_B_V,
        C_V = unique_C_V
      )
   
    min_RM <- RM_time_df[which.min(RM_time_df$rate), ]
    
    end_time <- subset(
      RM_time_df,
      RM_time_df$time >= min_RM$time &
        RM_time_df$rate >= 1
    )[1, "time"]
    
    
    df <- data.frame(
      endtime = end_time,
      up_down = "up",
      end_fitness = NA,
      status = "success"
    )
  }
  
  return(df)
}









FULL_MODEL_SIMULATING_Duration_criss_cross_Two <- function(variable_interest_1,
                                                           variable_interest_2,
                                                       initial_value,
                                                       C_V_opt ,
                                                       C_V_PC,
                                                       C_V_PF,
                                                       id = NA,
                                                       species,
                                                       include_death) {
  
  
  variable_interest_vec <- c(variable_interest_1, variable_interest_2)
  
  
  if (is.na(C_V_opt) == TRUE) {
    if (!("cv" %in% variable_interest_vec)) {
      C_V_val <- switch(species,
                        "PC" =  C_V_PC,
                        "PF" =  C_V_PF
      )
    } else if ( "cv" %in% variable_interest_vec ) {
      C_V_val <- switch(species,
                        "PC" =  C_V_PF,
                        "PF" =  C_V_PC
      )
    }
    else{
      C_V_val = C_V_opt
    }
  }
  
  ### Burst Size and Transmission Investment ###
  B_V <-seq(1, 50, 0.5) # Burst size
  
  # Different combinations
  B_V_C_V <- expand.grid(
    B_V = B_V,
    C_V = C_V_val,
    initialvalue = initial_value
  )
  
  
  ### We can already take out parameter combinations that would not
  ### lead to the establishment of infection
  
  RM_limit_1 <- Calculate_Initial_RM_CrissCross_Two(species, variable_interest_1,
                                                    variable_interest_2)
  
  B_V_C_V$Establish <- ifelse((1 - B_V_C_V$C_V) * B_V_C_V$B_V >= RM_limit_1,
                              "Establish", "Fail"
  )
  
  ### Simulate infections that are successful (may kill host!)
  B_V_C_V_F <- subset(B_V_C_V, B_V_C_V$Establish == "Establish")
  
  ### These infections are successful OR lead to host mortality
  
  model_sim <- switch(as.character(species),
                      "PC" = Simulator_PC_Criss_Cross_Two,
                      "PF" = Simulator_PF_Criss_Cross_Two
  )
  if (nrow(B_V_C_V_F) != 0) {
    
    FULL_MODEL <- mcmapply(model_sim,
                           c(variable_interest_1),
                           c(variable_interest_2),
                           c(B_V_C_V_F$B_V),
                           c(C_V_PC),
                           c(C_V_PF),
                           c(C_V_opt),
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
      paste("FULL_MODEL_DT", species, variable_interest_1,variable_interest_2,include_death, ".csv", sep = "")
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
                 Finder_RM_Two ,
                 species = species,
                 variable_interest_1 = variable_interest_1,
                 variable_interest_2 = variable_interest_2,
                 
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
                            "PC" = Simulator_PC_Criss_Cross_Cut_Two,
                            "PF" = Simulator_PF_Criss_Cross_Cut_Two
    )
    Fitness_MODEL <- mcmapply(model_sim_cut,
                              c(variable_interest_1),
                              c(variable_interest_2),
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
    
    Fitness_MODEL_FULL$variable_interest_1 <- variable_interest_1
    Fitness_MODEL_FULL$variable_interest_2 <- variable_interest_2
    
    ### Write into a CSV TO BE SAVED
    write.csv(Fitness_MODEL_FULL, file = here(
      "Output", "Fitness_Model",
      paste("FITNESS_MODEL_", species, variable_interest_1,variable_interest_2, include_death ,".csv", sep = "")
    ))
    
    
    return(Fitness_MODEL_FULL)
  } else {
    return(NA)
  }
}