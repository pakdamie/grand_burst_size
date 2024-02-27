##########################
### Main simulation code##
##########################
######################################################
###Please ensure that the pmax is 4.0e-6 AND that the#
### initial I (default) is 4358.965            #
############################################################
### Input: Burst size (B_V), transmission investment (C_V),#
### and the initial value (initialvalue)                   #
### Output: dataframe with only IRBC and Gametocytes       #                
##                                                         #  
############################################################
### Main modeling code
sourceCpp(here("Code", "Model", "rcpp", "rcpp_malaria_dynamics_CUT.cpp"))
sourceCpp(here("Code", "Model", "rcpp", "rcpp_malaria_dynamics_UNCUT.cpp"))

###This is the function that terminates the integrator early
### when the red blood cells reaches 6.5 * 10^5


Simulator_Malaria_BC_PC <- function(B_V, C_V, initialvalue) {
  
  rootfun <- function (t, y, parms) {return(y['R'] - 6.5*10^5)}
  
  parameters_n <-
    c(lambda = 370000, # Replenishment rate of RBC (#SimulatedTimeSeries.R)
      K = 19968254, # Carrying capacity of RBC population in the absence of mortality
      pmax = 4.0e-6, # Rate of infection (From American Naturalist- Greischar et al. 2014)
      muR = 0.025, # Daily mortality rate of red blood cells
      muI = 0.025, # Daily mortality rate of infected red blood cells
      c = C_V, # Transmission investment (THE VARYING FACTOR)
      B = B_V, # The burst size (THE VARYING FACTOR)
      alpha1 = 1, # The rate of development of parasite in iRBC
      alpha2 = 1/2, # The rate of development for the gametocytes
      muM = 48, # Background mortality of the merozoite
      muG = 4, # background mortality of the immature/mature gametocytes
      n1 = 100, # shape parameter controlling the variability in asexual devleopment
      n2 = 100 # shape parameter controlling the variability in sexual development
    )
  
  ### The number of subcompartments for infected rbc(n1)
  n1 <- parameters_n["n1"]
  ### The number of subcompartments for immature gametocytes
  n2 <- parameters_n["n2"]
  
  ### The initial numbers
  inits_n <-
    c(
    R = 8500000, # RBC
    I = rep(initialvalue/ n1, n1), # Infected RBC- note the uniform distribution
    M = 0, # merozoite
    IG = rep(0, n2), # immature gametocytes
    G = 0
  ) # gametocytes
  
  ### We just want to figure out when the peak infected RBC is at.
  times <- seq(0, 100, by = 1 / 10)
  
  
  out_DDE <- ode(
    y = inits_n,
    times = times,
    func = Erlang_Malaria,
    parms = parameters_n,
    rootfun = rootfun)
  
 return(data.frame(out_DDE[, c("time", "R", "G")], 
                   B_V = B_V, 
                   C_V = C_V,
                   initialvalue = initialvalue,
                   infection_length = 
                   ifelse(!is.null(attributes(out_DDE)$troot),
                          attributes(out_DDE)$troot,
                          NA)))

}


#################################################
### Input: Burst size and transmission investment##
### Output: deSolve class                        ##
##################################################

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
      muG = 4, # background mortality of the immature/mature gametocytes
      n1 = 100, # shape parameter controlling the variability in asexual devleopment
      n2 = 100, # shape parameter controlling the variability in sexual development
      endtime = endtime # When the asexual replication end
    )
  
  ### The number of subcompartments for infected rbc(n1)
  n1 <- parameters_n["n1"]
  ### The number of subcompartments for immature gametocytes
  n2 <- parameters_n["n2"]
  
  ### The initial numbers
  inits_n <- c(
    R = 8500000,
    I = rep(initialvalue/ n1, n1),
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
