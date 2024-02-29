###Function to help me create different  parameters to 
###criss-cross with  <- function(B_V, C_V, initialvalue) {

Simulator_PF_Criss_Cross <- function(variable_interest,B_V,C_V_opt,initialvalue){
  
  rootfun <- function (t, y, parms) {return(y['R'] - 380000)}
  
  ###if the variable_interest is NOT pmax_val than keep original-
  ###else, criss-cross with P. falciparum!
  pmax_val <- ifelse(variable_interest != "pmax", 8.35e-6, 4.0e-6)
  alpha1_val <- ifelse(variable_interest != "alpha1", 1/2, 1)
  alpha2_val <- ifelse(variable_interest != "alpha2", 1/7, 1/2)
  muM_val  <- ifelse(variable_interest != 'muM',200, 48)
  muG_val<- ifelse(variable_interest != 'muG', log(2)/2.4, 4)
  

  print(c(pmax_val,alpha1_val, alpha2_val, muM_val, muG_val))
  
  
  params_PF<- c(lambda = 2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
                    K = 6315789, # carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
                    pmax =  pmax_val , # rate of infection (From American Naturalist- Greischar et al. 2014)
                    muR = 1/120, #Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
                    muI = 1/120, #Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
                    c = C_V_opt, # transmission investment (Vary)
                    B = B_V, # the burst size (Vary)
                    alpha1 = alpha1_val, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
                    alpha2 = alpha2_val, #the rate of development (SimulatedTimeSeries.R)
                    muM = muM_val, # background mortality of the merozoite (SimulatedTimeSeries.R)
                    muG = muG_val, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
                    n1= 10, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
                    n2= 10, # background mortality of the merozoite (SimulatedTimeSeries.R)
                    a = 0,  #parameter associated with immune clearance (from Megan)
                    b= 0, #parameter associated with immune clearance (from Megan)
                    q= 0 #Merozite interference (eyeballed graph from From American Naturalist- Greischar et al. 2014 )
  )
  
  
  n1 <-params_PF["n1"]
  ### The number of subcompartments for immature gametocytes
  n2 <- params_PF["n2"]
  
  inits_n <- c(R =  5e6, 
               I = rep(initialvalue/n1, n1),
               M = 0,
               IG=rep(0,n2),
               G = 0)
  
  ### We just want to figure out when the peak infected RBC is at.
  times <- seq(0, 100, by = 1 / 10)
  
  
  out_DDE <- ode(
    y = inits_n,
    times = times,
    func = Erlang_Malaria,
    parms = params_PF,
    rootfun = rootfun)
  
  return(data.frame(out_DDE[, c("time", "R", "G")], 
                    B_V = B_V, 
                    C_V = C_V_opt,
                    initialvalue = initialvalue,
                    infection_length = 
                      ifelse(!is.null(attributes(out_DDE)$troot),
                             attributes(out_DDE)$troot,
                             NA)))
  
}
  

Simulator_PF_Criss_Cross_Cut <- function(variable_interest, B_V, C_V_opt,
                                         initialvalue, endtime) {
  
  ###if the variable_interest is NOT pmax_val than keep original-
  ###else, criss-cross with P. falciparum!
  pmax_val <- ifelse(variable_interest != "pmax",  8.35e-6, 4.0e-6 )
  alpha1_val <- ifelse(variable_interest != "alpha1",1/2, 1  )
  alpha2_val <- ifelse(variable_interest != "alpha2",1/7, 1/2  )
  muM_val  <- ifelse(variable_interest != 'muM', 200, 48 )
  muG_val<- ifelse(variable_interest != 'muG',  log(2)/2.4, 4)
  
  params_PF <- c(lambda = 2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
                    K = 6315789, # carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
                    pmax =  pmax_val  , # rate of infection (From American Naturalist- Greischar et al. 2014)
                    muR = 1/120, #Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
                    muI = 1/120, #Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
                    c = C_V_opt, # transmission investment (Vary)
                    B = B_V, # the burst size (Vary)
                    alpha1 = alpha1_val, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
                    alpha2 = alpha2_val, #the rate of development (SimulatedTimeSeries.R)
                    muM = muM_val, # background mortality of the merozoite (SimulatedTimeSeries.R)
                    muG =  muG_val, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
                    n1= 10, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
                    n2= 10, # background mortality of the merozoite (SimulatedTimeSeries.R)
                    a = 0,  #parameter associated with immune clearance (from Megan)
                    b= 0, #parameter associated with immune clearance (from Megan)
                    q= 0,
                    endtime = endtime)

  n1 <- params_PF ["n1"]
  ### The number of subcompartments for immature gametocytes
  n2 <- params_PF ["n2"]

  
  inits_n <- c(R = 5e6, 
               I = rep(initialvalue/n1, n1),
               M = 0,
               IG=rep(0,n2),
               G =0)
  
  ### We just want to figure out when the peak infected RBC is at.
  
  times <- seq(0, 100, by = 1 / 10)
  
  out_DDE <- ode(
    y = inits_n, 
    times = times,
    func = Erlang_Malaria_Cut,
    parms =  params_PF
  )
  
  return(data.frame(out_DDE[, c("time", "R", "G")], B_V = B_V, C_V = C_V_opt))
}
