### Title: Main simulation codes for Plasmodium falciparum (human)
###
###
### Input: Burst size (B_V), transmission investment (C_V),#
### and the initial, inoculum of parasite (initialvalue)
### Output: Dataframe with only IRBC and gametocytes

### Main modeling codes are written in C++, so if you would like to
### dive into the mathematics- look here:

sourceCpp(here("Code", "Model", "rcpp", "rcpp_malaria_dynamics_CUT.cpp"))
sourceCpp(here("Code", "Model", "rcpp", "rcpp_malaria_dynamics_UNCUT.cpp"))

Simulator_Malaria_BC_PF <- function(B_V, 
                                    C_V,  
                                    p_val,
                                    mu_M, 
                                    R_Modifier,
                                    initialvalue, 
                                    include_death) {
  
  parameters_n <- c(
    lambda = 2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
    K = 6315789, # carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
    pmax = p_val * 8.35e-6, # rate of infection (From American Naturalist- Greischar et al. 2014)
    muR = 1 / 120, # Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
    muI = 1 / 120, # Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
    c = C_V, # transmission investment (Vary)
    B = B_V, # the burst size (Vary)
    alpha1 = 1 / 2, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
    alpha2 = 1 / 7, # the rate of development (SimulatedTimeSeries.R)
    muM = mu_M * 200, # background mortality of the merozoite (SimulatedTimeSeries.R)
    muG = log(2) / 2.4, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
    n1 = 10, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
    n2 = 10, # background mortality of the merozoite (SimulatedTimeSeries.R)
    a = 0, # parameter associated with immune clearance (from Megan)
    b = 0, # parameter associated with immune clearance (from Megan)
    q = 0 # Merozite interference (eyeballed graph from From American Naturalist- Greischar et al. 2014 )
  )

  ### The number of subcompartments for infected RBCs
  n1 <- parameters_n["n1"]
  ### The number of subcompartments for Immature Gametocytes
  n2 <- parameters_n["n2"]

  ### The initial numbers
  inits_n <- c(
    R = R_Modifier * 5e6, # RBC
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
    return(y["R"] - 380000)
  }



  if (include_death == "No") {
    ### Does not include the rootfun

    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = parameters_n
    )
  } else {
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
    p_val = p_val,
    mu_M = mu_M,
    R_Modifier = R_Modifier,
    infection_length =
      ifelse(!is.null(attributes(out_DDE)$troot),
        attributes(out_DDE)$troot,
        NA
      )
  ))
}

### This function simulates to the end of the acute phase, you only
### run this when you know when the acute phase ends from the prior analyses


Simulator_MalariaPF_DDE_BC_Cut <- function(B_V, C_V,  p_val,
                                           mu_M, R_Modifier, initialvalue, endtime) {
  parameters_n <-
    c(
      lambda = 2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
      K = 6315789, # carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
      pmax = p_val * 8.35e-6, # rate of infection (From American Naturalist- Greischar et al. 2014)
      muR = 1 / 120, # Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
      muI = 1 / 120, # Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
      c = C_V, # transmission investment (Vary)
      B = B_V, # the burst size (Vary)
      alpha1 = 1 / 2, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
      alpha2 = 1 / 7, # the rate of development (SimulatedTimeSeries.R)
      muM = mu_M * 200, # background mortality of the merozoite (SimulatedTimeSeries.R)
      muG = log(2) / 2.4, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
      n1 = 10, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
      n2 = 10, # background mortality of the merozoite (SimulatedTimeSeries.R)
      a = 0, # parameter associated with immune clearance (from Megan)
      b = 0, # parameter associated with immune clearance (from Megan)
      q = 0, # Merozite interference (eyeballed graph from From American Naturalist- Greischar et al. 2014 )
      endtime = endtime
    )

  ### The number of subcompartments for infected RBCs
  n1 <- parameters_n["n1"]
  ### The number of subcompartments for Immature Gametocytes
  n2 <- parameters_n["n2"]

  ### The initial number
  inits_n <- c(
    R = R_Modifier * 5e6, # RBC
    I = rep(initialvalue / n1, n1), # Infected RBC: note the uniform distribution
    M = 0, # Merozoites
    IG = rep(0, n2), # Immature gametocytes
    G = 0 # Gametocytes
  )

  ### Daily time steps
  times <- seq(0, 100, by = 1 / 10)


  out_DDE <- ode(
    y = inits_n, times = times,
    func = Erlang_Malaria_Cut,
    parms = parameters_n
  )


  return(data.frame(out_DDE[, c("time", "R", "G")], B_V = B_V, C_V = C_V,p_val = p_val,mu_M = mu_M, R_Modifier = R_Modifier))
}
