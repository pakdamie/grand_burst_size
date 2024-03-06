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
sourceCpp(here("Code", "Model","rcpp", "rcpp_malaria_dynamics_CUT.cpp"))
sourceCpp(here("Code", "Model","rcpp", "rcpp_malaria_dynamics_UNCUT.cpp"))

###This is the function that terminates the integrator early
### when the red blood cells reaches 7.6 percent of the initial
###RBC, using the same proportion as the mouse data


Simulator_Malaria_BC_PF <- function(B_V, C_V, initialvalue,include_death){
  rootfun <- function (t, y, parms) {return(y['R'] - 380000)}
  
   parameters_n <- c(lambda = 2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
                    K = 6315789, # carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
                    pmax =  8.35e-6 , # rate of infection (From American Naturalist- Greischar et al. 2014)
                    muR = 1/120, #Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
                    muI = 1/120, #Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
                    c = C_V, # transmission investment (Vary)
                    B = B_V, # the burst size (Vary)
                    alpha1 = 1/2, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
                    alpha2 = 1/7, #the rate of development (SimulatedTimeSeries.R)
                    muM = 200, # background mortality of the merozoite (SimulatedTimeSeries.R)
                    muG = log(2)/2.4, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
                    n1= 10, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
                    n2= 10, # background mortality of the merozoite (SimulatedTimeSeries.R)
                    a = 0,  #parameter associated with immune clearance (from Megan)
                    b= 0, #parameter associated with immune clearance (from Megan)
                    q= 0 #Merozite interference (eyeballed graph from From American Naturalist- Greischar et al. 2014 )
  )
  
  
  n1=parameters_n['n1'];
  n2=parameters_n['n2']
  
  inits_n <- c(R = 5e6, 
               I = rep(initialvalue/n1, n1),
               M = 0,
               IG=rep(0,n2),
               G =0)
  
  ### We just want to figure out when the peak infected RBC is at.
  times <- seq(0, 100, by = 1 / 10)
  
  if(include_death == "No"){
    
    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = parameters_n)
  }
  else{
    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = parameters_n,
      rootfun = rootfun)
  }
  return(data.frame(out_DDE[, c("time", "R", "G")], 
                    B_V = B_V, 
                    C_V = C_V,
                    initialvalue = initialvalue,
                    infection_length = 
                      ifelse(!is.null(attributes(out_DDE)$troot),
                             attributes(out_DDE)$troot,
                             NA)))
  
}


Simulator_MalariaPF_DDE_BC_Cut <- function(B_V, C_V, initialvalue, endtime) {
  parameters_n <-
    c(lambda =2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
      K = 6315789, # carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
      pmax =  8.35e-6 , # rate of infection (From American Naturalist- Greischar et al. 2014)
      muR = 1/120, #Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
      muI =1/120, #Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
      c = C_V, # transmission investment (Vary)
      B = B_V, # the burst size (Vary)
      alpha1 = 1/2, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
      alpha2 = 1/7, #the rate of development (SimulatedTimeSeries.R)
      muM = 200, # background mortality of the merozoite (SimulatedTimeSeries.R)
      muG =  log(2)/2.4, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
      n1= 10, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
      n2= 10, # background mortality of the merozoite (SimulatedTimeSeries.R)
      a = 0,  #parameter associated with immune clearance (from Megan)
      b= 0, #parameter associated with immune clearance (from Megan)
      q= 0, #Merozite interference (eyeballed graph from From American Naturalist- Greischar et al. 2014 )
      endtime = endtime
    )
  
  
  n1=parameters_n['n1'];
  n2=parameters_n['n2']
  
  inits_n <- c(R =  5e6, 
               I = rep(initialvalue/n1, n1),
               M = 0,
               IG=rep(0,n2),
               G =0)
  
  
  ### We just want to figure out when the peak infected RBC is at.
  times <- seq(0, 100, by = 1 / 10)
  
  
  out_DDE <- ode(
    y = inits_n, times = times,
    func = Erlang_Malaria_Cut,
    parms = parameters_n
  )
  
  
  return(data.frame(out_DDE[, c("time", "R", "G")], B_V = B_V, C_V = C_V))
  
}
