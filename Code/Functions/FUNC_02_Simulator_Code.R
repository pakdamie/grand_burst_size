###########################################################
###THIS IS THE FUNCTION THAT SIMULATES INFECTIONS         #
###AND TELLS YOU THE FITNESS AT THE END OF THE ACUTE PHASE#
###########################################################
###This calculates the initial RM that allows for the parasites
###to establish. Species include "PC" or "PF" and criss_cross
###indicate that the parameters are switched.

Calculate_Initial_RM <- function(species, criss_cross) {
  initial_RM_modifier <- 1.5 # The threshold for establishment.
  if (criss_cross == "NO") { # If we're not criss-crossing the parameter values

    time_delayer <- switch(species,
      "PC" = ((100 * (1 / 1) + (1 / 40))^100) / (100 * (1 / 1))^100,
      "PF" = ((10 * (1 / 2) + (1 / 120))^10) / (10 * (1 / 2))^10
    )

    p_val <- switch(species,
      "PC" = 4.0e-6,
      "PF" = 8.35e-6
    )

    mu_M <- switch(species,
      "PC" = 48,
      "PF" = 200
    )

    R_val <- switch(species,
      "PC" = 8500000,
      "PF" = 5e6
    )


    RM_limit_1 <- initial_RM_modifier * time_delayer * ((p_val * R_val) + mu_M) / (p_val * R_val)
 }else if (criss_cross == "YES") {
    time_delayer_CROSSED <- switch(species,
      "PC" = ((100 * (1 / 2) + (1 / 40))^100) / (100 * (1 / 2))^100,
      "PF" = ((10 * (1 / 1) + (1 / 120))^10) / (10 * (1 / 1))^10
    )

    p_val_CROSSED <- switch(species,
      "PC" = 8.35e-6,
      "PF" = 4.0e-6
    )

    mu_M_CROSSED <- switch(species,
      "PC" = 200,
      "PF" = 48
    )

    R_val <- switch(species,
      "PC" = 8500000,
      "PF" = 5e6
    )


    RM_limit_1 <- initial_RM_modifier * time_delayer_CROSSED * ((p_val_CROSSED * R_val) + mu_M) / (p_val_CROSSED * R_val)
     }
  return( RM_limit_1 )
}


#Input: 
#1)initial_value - The different intial values for the infected RBC inoculum
#2) mu_M_value - the mortality rate of merozoite, crucial for figuring out
# which infections are unestablished
#3 id - give it a low, med, high id


###SIMULATE THE MODEL
FULL_MODEL_SIMULATING_Duration <- function(initial_value, id, species){
 
  R_0 = seq(1,50,0.5) #For chabaudi 
  R_0_SQUARED = R_0^2 #For falciparum
  C_V = seq(.1,1,0.1)
  ##########################################################
  ###THE FIRST PART OF THE MODEL, trying to figure out what#
  ###parameter combinations we want to be looking at       #
  ##########################################################
  ### Burst Size and Transmission Investment ###
  C_V <- seq(.01, 1, 0.01) # Transmission investment
  
  B_V <-switch(species,
       "PC"  =   R_0 ,
       "PF" = R_0_SQUARED )  
  
  B_V_C_V <- expand.grid(B_V = B_V, 
                         C_V = C_V, 
                         initialvalue = initial_value) # Different combinations
  
  
  ###We can already take out parameter combinations that would not
  ###lead to the establishment of infection
  
  RM_limit_1 <- Calculate_Initial_RM(species,"NO")
  
  B_V_C_V$Establish <- ifelse((1 - B_V_C_V$C_V) * B_V_C_V$B_V >= RM_limit_1,
                            "Establish", "Fail")

  ### Simulate infections that are successful (may kill host!)
  B_V_C_V_F <- subset(B_V_C_V, B_V_C_V$Establish == "Establish")

  ### These infections are successful OR lead to host mortality
 
  model_sim <- switch(species,
                      "PC" = Simulator_Malaria_BC_PC,
                      "PF" = Simulator_Malaria_BC_PF 
                      
  )
  
  
   FULL_MODEL <-  mcmapply(model_sim,
                          c(B_V_C_V_F$B_V),
                          c(B_V_C_V_F$C_V),
                          c(B_V_C_V_F$initialvalue),
                          mc.cores = 5,
                          SIMPLIFY = FALSE)
  
  ### Combine all list elements to make it easier to save/read
  FULL_MODEL_DT <- do.call(rbind, FULL_MODEL)
  
  ### Write into a CSV TO BE SAVED
  write.csv(FULL_MODEL_DT, file = here(
    "Output", "Full_Model",
    paste("FULL_MODEL_DT_",species,".csv")))
  
  ###REMOVE right now to save space
  remove( FULL_MODEL_DT)
  
  
  #########################################################
  ###THIS THEN FIGURES OUT THE DURATION OF THE ACUTE PHASE#
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
  
  
  ###Now we can find the parameter combinations that lead to 
  ###unestablished infection- we then set the values 
  ###manually to 9
  
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
    unlist(lapply(Fitness_MODEL, Gametocyte_Fitness, species = "PF"))
  
  ### These are the fitness model data.frame that should work
  Fitness_MODEL_FULL <- rbind.data.frame(
    Duration_Initial_SUCCESS,
    Duration_Initial_FAIL,
    Duration_Initial_MORT
  )
  
  ### Write into a CSV TO BE SAVED 
  write.csv(Fitness_MODEL_FULL, file = here(
    "Output", "Fitness_Model",
    paste("FITNESS_MODEL_",species,".csv", sep = "")))
  
  
  return(Fitness_MODEL_FULL)

}



