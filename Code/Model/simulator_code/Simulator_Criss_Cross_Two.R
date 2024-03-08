### Function to help me create different  parameters to
### criss-cross with  <- function(B_V, C_V, initialvalue) {

Simulator_PF_Criss_Cross_Two <- function(variable_interest_1,
                                         variable_interest_2,
                                     B_V,
                                     CV_PC,
                                     CV_PF,
                                     C_V_opt = NA,
                                     initialvalue,
                                     include_death) {
  rootfun <- function(t, y, parms) {
    return(y["R"] - 380000)
  }
  
  
  variable_interest_vec <- c(variable_interest_1, variable_interest_2)
  
  ### if the variable_interest is NOT pmax_val than keep original-
  ### else, criss-cross with P. falciparum!
  pmax_val <- ifelse(!("pmax" %in% variable_interest_vec), 8.35e-6, 4.0e-6)
  alpha1_val <- ifelse(!("alpha1" %in% variable_interest_vec), 1 / 2, 1)
  alpha2_val <- ifelse(!("alpha2" %in% variable_interest_vec), 1 / 7, 1 / 2)
  muM_val <- ifelse(!("muM" %in% variable_interest_vec), 200, 48)
  muG_val <- ifelse(!("muG"%in% variable_interest_vec), log(2) / 2.4, 4)
  
  
  
  
  if (is.na(C_V_opt) == TRUE) {
    C_V_val <- ifelse(!("cv" %in% variable_interest_vec), CV_PF, CV_PC)
  } else {
    C_V_val <- C_V_opt
  }
  
  params_PF <- c(
    lambda = 2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
    K = 6315789, # carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
    pmax = pmax_val, # rate of infection (From American Naturalist- Greischar et al. 2014)
    muR = 1 / 120, # Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
    muI = 1 / 120, # Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
    c = C_V_val, # transmission investment (Vary)
    B = B_V, # the burst size (Vary)
    alpha1 = alpha1_val, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
    alpha2 = alpha2_val, # the rate of development (SimulatedTimeSeries.R)
    muM = muM_val, # background mortality of the merozoite (SimulatedTimeSeries.R)
    muG = muG_val, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
    n1 = 10, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
    n2 = 10, # background mortality of the merozoite (SimulatedTimeSeries.R)
    a = 0, # parameter associated with immune clearance (from Megan)
    b = 0, # parameter associated with immune clearance (from Megan)
    q = 0 # Merozite interference (eyeballed graph from From American Naturalist- Greischar et al. 2014 )
  )
  
  n1 <- params_PF["n1"]
  ### The number of subcompartments for immature gametocytes
  n2 <- params_PF["n2"]
  
  inits_n <- c(
    R = 5e6,
    I = rep(initialvalue / n1, n1),
    M = 0,
    IG = rep(0, n2),
    G = 0
  )
  
  ### We just want to figure out when the peak infected RBC is at.
  times <- seq(0, 100, by = 1 / 10)
  
  
  if(include_death == "No"){
    
    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = params_PF)
  }
  else{
    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = params_PF,
      rootfun = rootfun)
  }
  
  return(data.frame(out_DDE[, c("time", "R", "G")],
                    B_V = B_V,
                    C_V =  C_V_val,
                    initialvalue = initialvalue,
                    infection_length =
                      ifelse(!is.null(attributes(out_DDE)$troot),
                             attributes(out_DDE)$troot,
                             NA
                      )
  ))
}





Simulator_PF_Criss_Cross_Cut_Two <- function(variable_interest_1, 
                                             variable_interest_2,
                                             B_V, CV_PC,
                                         CV_PF, C_V_opt = NA,
                                         initialvalue, endtime) {
  
  variable_interest_vec <- c(variable_interest_1, variable_interest_2)
  
  ### if the variable_interest is NOT pmax_val than keep original-
  ### else, criss-cross with P. falciparum!
  pmax_val <- ifelse(!("pmax" %in% variable_interest_vec), 8.35e-6, 4.0e-6)
  alpha1_val <- ifelse(!("alpha1" %in% variable_interest_vec), 1 / 2, 1)
  alpha2_val <- ifelse(!("alpha2" %in% variable_interest_vec), 1 / 7, 1 / 2)
  muM_val <- ifelse(!("muM" %in% variable_interest_vec), 200, 48)
  muG_val <- ifelse(!("muG"%in% variable_interest_vec), log(2) / 2.4, 4)
  
  

  
  if (is.na(C_V_opt) == TRUE) {
    C_V_val <- ifelse(!("cv" %in% variable_interest_vec), CV_PF, CV_PC)
  } else {
    C_V_val <- C_V_opt
  }
  
  params_PF <- c(
    lambda = 2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
    K = 6315789, # carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
    pmax = pmax_val, # rate of infection (From American Naturalist- Greischar et al. 2014)
    muR = 1 / 120, # Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
    muI = 1 / 120, # Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
    c = C_V_val, # transmission investment (Vary)
    B = B_V, # the burst size (Vary)
    alpha1 = alpha1_val, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
    alpha2 = alpha2_val, # the rate of development (SimulatedTimeSeries.R)
    muM = muM_val, # background mortality of the merozoite (SimulatedTimeSeries.R)
    muG = muG_val, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
    n1 = 10, # shape parameter controlling the variability in (SimulatedTimeSeries.R)
    n2 = 10, # background mortality of the merozoite (SimulatedTimeSeries.R)
    a = 0, # parameter associated with immune clearance (from Megan)
    b = 0, # parameter associated with immune clearance (from Megan)
    q = 0,
    endtime = endtime
  )
  
  n1 <- params_PF["n1"]
  ### The number of subcompartments for immature gametocytes
  n2 <- params_PF["n2"]
  
  
  inits_n <- c(
    R = 5e6,
    I = rep(initialvalue / n1, n1),
    M = 0,
    IG = rep(0, n2),
    G = 0
  )
  
  ### We just want to figure out when the peak infected RBC is at.
  
  times <- seq(0, 100, by = 1 / 10)
  
  out_DDE <- ode(
    y = inits_n,
    times = times,
    func = Erlang_Malaria_Cut,
    parms = params_PF
  )
  
  return(data.frame(out_DDE[, c("time", "R", "G")], B_V = B_V, C_V =  C_V_val))
}


###Function to help me create different  parameters to 
###criss-cross with  <- function(B_V, C_V, initialvalue) {

Simulator_PC_Criss_Cross_Two <- function(variable_interest_1,
                                         variable_interest_2,
                                     B_V,
                                     CV_PC,
                                     CV_PF,
                                     C_V_opt = NA,
                                     initialvalue,
                                     include_death){
  
  rootfun <- function (t, y, parms) {return(y['R'] - 6.5*10^5)}
  
  variable_interest_vec <- c(variable_interest_1, variable_interest_2)
  
  ###if the variable_interest is NOT pmax_val than keep original-
  ###else, criss-cross with P. falciparum!
  pmax_val <- ifelse(!("pmax" %in% variable_interest_vec), 4.0e-6,8.35e-6)
  alpha1_val <- ifelse(!("alpha1" %in% variable_interest_vec), 1,  1 / 2)
  alpha2_val <- ifelse(!("alpha2" %in% variable_interest_vec),  1 / 2, 1/7)
  muM_val <- ifelse(!("muM" %in% variable_interest_vec),48, 200)
  muG_val <- ifelse(!("muG"%in% variable_interest_vec), 4, log(2) / 2.4)
  
  
  if (is.na(C_V_opt) == TRUE) {
    C_V_val <- ifelse(!("cv"%in%variable_interest_vec) , CV_PC, CV_PF)
  } else {
    C_V_val <- C_V_opt
  }
  params_PC <- 
    c(lambda = 370000, # Replenishment rate of RBC (#SimulatedTimeSeries.R)
      K = 19968254, # Carrying capacity of RBC population in the absence of mortality
      pmax =   pmax_val , # Rate of infection (From American Naturalist- Greischar et al. 2014)
      muR = 0.025, # Daily mortality rate of red blood cells
      muI = 0.025, # Daily mortality rate of infected red blood cells
      c = C_V_val, # Transmission investment (THE VARYING FACTOR)
      B = B_V, # The burst size (THE VARYING FACTOR)
      alpha1 = alpha1_val, # The rate of development of parasite in iRBC
      alpha2 =alpha2_val, # The rate of development for the gametocytes
      muM =  muM_val, # Background mortality of the merozoite
      muG = muG_val, # background mortality of the immature/mature gametocytes
      n1 = 100, # shape parameter controlling the variability in asexual devleopment
      n2 = 100 # shape parameter controlling the variability in sexual development
    )
  
  n1 <-  params_PC ["n1"]
  ### The number of subcompartments for immature gametocytes
  n2 <-  params_PC ["n2"]
  
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
  
  if(include_death == "No"){
    
    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = params_PC)
  }
  else{
    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = params_PC,
      rootfun = rootfun)
  }
  
  
  
  return(data.frame(out_DDE[, c("time", "R", "G")], 
                    B_V = B_V, 
                    C_V =  C_V_val,
                    initialvalue = initialvalue,
                    infection_length = 
                      ifelse(!is.null(attributes(out_DDE)$troot),
                             attributes(out_DDE)$troot,
                             NA)))
  
}



Simulator_PC_Criss_Cross_Cut <- function(variable_interest_1,
                                         variable_interest_2, 
                                         B_V, 
                                         CV_PC,
                                         CV_PF,
                                         C_V_opt = NA,
                                         initialvalue, 
                                         endtime) {
  
  rootfun <- function (t, y, parms) {return(y['R'] - 6.5*10^5)}
  variable_interest_vec <- c(variable_interest_1, variable_interest_2)
  
  ###if the variable_interest is NOT pmax_val than keep original-
  ###else, criss-cross with P. falciparum!
  ###if the variable_interest is NOT pmax_val than keep original-
  ###else, criss-cross with P. falciparum!
  pmax_val <- ifelse(!("pmax" %in% variable_interest_vec), 4.0e-6,8.35e-6)
  alpha1_val <- ifelse(!("alpha1" %in% variable_interest_vec), 1,  1 / 2)
  alpha2_val <- ifelse(!("alpha2" %in% variable_interest_vec),  1 / 2, 1/7)
  muM_val <- ifelse(!("muM" %in% variable_interest_vec),48, 200)
  muG_val <- ifelse(!("muG"%in% variable_interest_vec), 4, log(2) / 2.4)
  
  
  if (is.na(C_V_opt) == TRUE) {
    C_V_val <- ifelse(!("cv"%in%variable_interest_vec), CV_PC, CV_PF)
  } else {
    C_V_val <- C_V_opt
  }
  
  params_PC <- 
    c(lambda = 370000, # Replenishment rate of RBC (#SimulatedTimeSeries.R)
      K = 19968254, # Carrying capacity of RBC population in the absence of mortality
      pmax =   pmax_val , # Rate of infection (From American Naturalist- Greischar et al. 2014)
      muR = 0.025, # Daily mortality rate of red blood cells
      muI = 0.025, # Daily mortality rate of infected red blood cells
      c = C_V_val, # Transmission investment (THE VARYING FACTOR)
      B = B_V, # The burst size (THE VARYING FACTOR)
      alpha1 = alpha1_val, # The rate of development of parasite in iRBC
      alpha2 =alpha2_val, # The rate of development for the gametocytes
      muM =  muM_val, # Background mortality of the merozoite
      muG = muG_val, # background mortality of the immature/mature gametocytes
      n1 = 100, # shape parameter controlling the variability in asexual devleopment
      n2 = 100,# shape parameter controlling the variability in sexual development
      endtime = endtime
    )
  
  n1 <-  params_PC ["n1"]
  ### The number of subcompartments for immature gametocytes
  n2 <-  params_PC ["n2"]
  
  ### The initial numbers
  inits_n <-
    c(
      R = 8500000, # RBC
      I = rep(initialvalue/n1, n1), # Infected RBC- note the uniform distribution
      M = 0, # merozoite
      IG = rep(0, n2), # immature gametocytes
      G = 0
    ) # gametocytes
  
  ### We just want to figure out when the peak infected RBC is at.
  times <- seq(0, 100, by = 1 / 10)
  
  
  out_DDE <- ode(
    y = inits_n, times = times,
    func = Erlang_Malaria_Cut,
    parms =  params_PC
  )
  
  return(data.frame(out_DDE[, c("time", "R", "G")], B_V = B_V, C_V = C_V_val))
}


