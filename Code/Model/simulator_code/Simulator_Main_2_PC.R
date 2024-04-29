### Title: Main simulation codes for Plasmodium chabaudi
### Notes: Please ensure that the pmax is 4.0e-6 AND that the
### initial (default) is 4358.965            #
###
### Input: Burst size (B_V), transmission investment (C_V),#
### and the initial, inoculum of parasite (initialvalue)
### Output: Dataframe with only IRBC and gametocytes

### Main modeling codes are written in C++, so if you would like to 
### dive into the mathematics- look here:
sourceCpp(here("Code", "Model", "rcpp", "rcpp_malaria_dynamics_CUT.cpp"))
sourceCpp(here("Code", "Model", "rcpp", "rcpp_malaria_dynamics_UNCUT.cpp"))

### This function simulates to Day 100 unless there is death.
Simulator_Malaria_BC_PC <- function(B_V, C_V, initialvalue, include_death) {
  parameters_n <-
    c(
      lambda = 370000, # Replenishment rate of RBC (#SimulatedTimeSeries.R)
      K = 19968254, # Carrying capacity of RBC population in the absence of mortality
      pmax = 4.0e-6, # Rate of infection (From American Naturalist- Greischar et al. 2014)
      muR = 0.025, # Daily mortality rate of red blood cells
      muI = 0.025, # Daily mortality rate of infected red blood cells
      c = C_V, # Transmission investment (THE VARYING FACTOR)
      B = B_V, # The burst size (THE VARYING FACTOR)
      alpha1 = 1, # The rate of development of parasite in iRBC
      alpha2 = 1 / 2, # The rate of development for the gametocytes
      muM = 48, # Background mortality of the merozoite
      muG = 4, # Background mortality of the immature/mature gametocytes
      n1 = 100, # Shape parameter controlling the variability in asexual devleopment
      n2 = 100 # Shape parameter controlling the variability in sexual development
    )

  ### The number of subcompartments for infected RBCs
  n1 <- parameters_n["n1"]
  ### The number of subcompartments for Immature Gametocytes
  n2 <- parameters_n["n2"]

  ### The initial numbers
  inits_n <-
    c(
      R = 8500000, # RBC
      I = rep(initialvalue / n1, n1), # Infected RBC: note the uniform distribution
      M = 0, # Merozoites
      IG = rep(0, n2), # Immature gametocytes
      G = 0 # Gametocytes
    ) 

  ### Daily time steps
  times <- seq(0, 100, by = 1 / 10)

  ### Mortality function: If the red blood cells hit this threshold,
  ### end the simulation to this point.

  rootfun <- function(t, y, parms) {
    return(y["R"] - 6.5 * 10^5)
  }

  if (include_death == "No") {
    ### Does not include the rootfun
    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = parameters_n
    )
  } else if (include_death == "Yes") {
    # Includes the rootfun
    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = parameters_n,
      rootfun = rootfun
    )
  }


  return(data.frame(out_DDE[, c("time", "R", "G")],
    B_V = B_V,
    C_V = C_V,
    initialvalue = initialvalue,
    infection_length =
      ifelse(!is.null(attributes(out_DDE)$troot),
        attributes(out_DDE)$troot,
        NA
      )
  ))
}


### This function simulates to the end of the acute phase, you only
### run this when you know when the acute phase ends from the prior analyses

Simulator_MalariaPC_DDE_BC_Cut <- function(B_V, C_V, initialvalue, endtime) {
  parameters_n <-
    c(
      lambda = 370000, # Replenishment rate of RBC (#SimulatedTimeSeries.R)
      K = 19968254, # Carrying capacity of RBC population in the absence of mortality
      pmax = 4.0e-6, # Rate of infection (From American Naturalist- Greischar et al. 2014)
      muR = 0.025, # Daily mortality rate of red blood cells
      muI = 0.025, # Daily mortality rate of infected red blood cells
      c = C_V, # Transmission investment (THE VARYING FACTOR)
      B = B_V, # The burst size (THE VARYING FACTOR)
      alpha1 = 1, # The rate of development of parasite in iRBC
      alpha2 = 1 / 2, # The rate of development
      muM = 48, # Background mortality of the merozoite
      muG = 4, # Background mortality of the immature/mature gametocytes
      n1 = 100, # Shape parameter controlling the variability in asexual devleopment
      n2 = 100, # Shape parameter controlling the variability in sexual development
      endtime = endtime # When the acute phase ends (And thus the asexual cycle)
    )

  ### The number of subcompartments for infected rbc(n1)
  n1 <- parameters_n["n1"]
  ### The number of subcompartments for immature gametocytes
  n2 <- parameters_n["n2"]

  ### The initial numbers
  inits_n <- c(
    R = 8500000,
    I = rep(initialvalue / n1, n1),
    M = 0,
    IG = rep(0, n2),
    G = 0
  )

  times <- seq(0, 100, by = 1 / 10)

  out_DDE <- ode(
    y = inits_n, times = times,
    func = Erlang_Malaria_Cut,
    parms = parameters_n
  )

  return(data.frame(out_DDE[, c("time", "R", "G")], B_V = B_V, C_V = C_V))
}
