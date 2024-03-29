########################################
### These are the functions that you use#
### to quantify fitness for all analyses#
########################################


### Calculates the fitness function of Plasmodium chabaudi based on gametocyte
### numbers
### Function: Daily transmission calculator based on gametocyte density
### for Plasmodium chabaudi
### Input: Gametocyte density
### Output: Probability of transmitting to a mosquito
PrI_PC <- function(G) {
  exp(-12.69 + 3.6 * log10(G)) / (1 + exp(-12.69 + 3.6 * log10(G)))
}

### Calculates the fitness function of Plasmodium falciparum based on gametocyte
### numbers
### Input: Gametocyte density
### Output: Probability of transmitting to a mosquito
PrI_PF <- function(gamDens) {
  # this function returns the probability of infection as a function of gametocyte density
  # parameter values taken for q2 in Huijben et al. 2010 Evolution
  alphaVal <- 0.03
  betaVal <- 0.6
  gammaVal <- alphaVal / 0.85
  N <- gamDens
  q <- alphaVal * (N^betaVal) / (1 + gammaVal * (N^betaVal))
  return(q)
}


### A more generalizable function that you can choose which fitness functions
### to use.
### Input: Data.frame of all the life-stages from the de-solve simulator #
### Output: The maximum cumulative transmission potential                #
Gametocyte_Fitness <- function(x, species) {
  
  gam_dat <- data.frame(
    time = x["time"],
    gam = x[, "G"]
  )

  ### If it's negative then, make it 0 just in case
  gam_dat$gam[gam_dat$gam < 0] <- 0

  fitness_func <- switch(species,
    "PC" = PrI_PC,
    "PF" = PrI_PF
  )

  end_fitness <- max(cumsum(fitness_func(gam_dat$gam) * 1 / 10))

  return(end_fitness)
}


### Calculate the RM
RM_Calculator <- function(species, x_list) {
  time_delayer <- switch(species,
    "PC" = ((100 * (1))^100) / ((100 * (1) + 0.025))^100,
    "PF" = ((10 * (1 / 2))^10 )/ ((10 * (1 / 2) + (1 / 120)))^10
  )
  
  p_val <- switch(species,
    "PC" = 4.0e-6,
    "PF" = 8.35e-6
  )

  mu_M <- switch(species,
    "PC" = 48,
    "PF" = 200
  )

  unique_B_V <- unique(x_list$B_V)
  unique_C_V <- unique(x_list$C_V)

  rate <- time_delayer * (1 - unique_C_V) * unique_B_V *
    ((x_list[, "R"] * p_val) / ((p_val * x_list[, "R"]) + mu_M))

  return(rate)
}


RM_Calculator_Criss_Cross <- function(species, x_list, variable_interest) {
  if (species == "PC") {
    pmax_val <- ifelse(variable_interest != "pmax", 4.0e-6, 8.35e-6)
    alpha1_val <- ifelse(variable_interest != "alpha1", 1, 1 / 2)
    muM_val <- ifelse(variable_interest != "muM", 48, 200)
    PC_Time_Delayer <- ((100 * (alpha1_val))^100) / ((100 * (alpha1_val) + 0.025))^100

    unique_B_V <- unique(x_list$B_V)
    unique_C_V <- unique(x_list$C_V)

    rate <- PC_Time_Delayer * (1 - unique_C_V) * unique_B_V *
      ((x_list[, "R"] * pmax_val) / ((pmax_val * x_list[, "R"]) + muM_val))
    
  } else if (species == "PF") {
    
    pmax_val <- ifelse(variable_interest!= "pmax", 8.35e-6, 4.0e-6)
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
Finder_RM <- function(x_list, species, criss_cross, variable_interest) {
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


    criss_cross_RM <- switch(criss_cross,
      "NO" = RM_Calculator,
      "YES" = RM_Calculator_Criss_Cross
    )


    if (criss_cross == "YES") {
      RM_time_df <- cbind.data.frame(
        time = x_list[, "time"],
        rate = criss_cross_RM(species, x_list, variable_interest),
        B_V = unique_B_V,
        C_V = unique_C_V
      )
    } else if (criss_cross == "NO") {
      RM_time_df <- cbind.data.frame(
        time = x_list[, "time"],
        rate = criss_cross_RM(species, x_list),
        B_V = unique_B_V,
        C_V = unique_C_V
      )
    }

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

