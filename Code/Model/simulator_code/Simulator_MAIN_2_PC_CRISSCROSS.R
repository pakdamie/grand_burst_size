###Function to help me create different  parameters to 
###criss-cross with  <- function(B_V, C_V, initialvalue) {

Simulator_PC_Criss_Cross <- function(value_interest, B_V, C_V_opt,initialvalue){
  
  rootfun <- function (t, y, parms) {return(y['R'] - 6.5*10^5)}
  
  ###if the variable_interest is NOT pmax_val than keep original-
  ###else, criss-cross with P. falciparum!
  pmax_val <- ifelse(value_interest != "pmax", 4.0e-6, 8.35e-6  )
  alpha1_val <- ifelse(value_interest != "alpha1", 1 , 1/2 )
  alpha2_val <- ifelse(value_interest != "alpha2", 1/2 , 1/7 )
  muM_val  <- ifelse(value_interest!= 'muM', 48, 200 )
  muG_val<- ifelse(value_interest != 'muG', 4,  log(2)/2.4)

  params_PC <- 
    c(lambda = 370000, # Replenishment rate of RBC (#SimulatedTimeSeries.R)
      K = 19968254, # Carrying capacity of RBC population in the absence of mortality
      pmax =   pmax_val , # Rate of infection (From American Naturalist- Greischar et al. 2014)
      muR = 0.025, # Daily mortality rate of red blood cells
      muI = 0.025, # Daily mortality rate of infected red blood cells
      c = C_V_opt, # Transmission investment (THE VARYING FACTOR)
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
    
    out_DDE <- ode(
      y = inits_n,
      times = times,
      func = Erlang_Malaria,
      parms = params_PC,
      rootfun = rootfun)
    
    return(data.frame(out_DDE[, c("time", "R", "G")], 
                      B_V = B_V, 
                      C_V =  C_V_opt,
                      initialvalue = initialvalue,
                      infection_length = 
                        ifelse(!is.null(attributes(out_DDE)$troot),
                               attributes(out_DDE)$troot,
                               NA)))
    
}
    


Simulator_PC_Criss_Cross_Cut <- function(variable_interest, B_V, C_V_opt,
                                         initialvalue, endtime) {
  
  rootfun <- function (t, y, parms) {return(y['R'] - 6.5*10^5)}
  
  ###if the variable_interest is NOT pmax_val than keep original-
  ###else, criss-cross with P. falciparum!
  pmax_val <- ifelse(variable_interest != "pmax",4.0e-6, 8.35e-6  )
  alpha1_val <- ifelse(variable_interest != "alpha1", 1 , 1/2 )
  alpha2_val <- ifelse(variable_interest != "alpha2", 1/2 , 1/7 )
  muM_val  <- ifelse(variable_interest != 'muM', 48, 200 )
  muG_val<- ifelse(variable_interest != 'muG', 4,  log(2)/2.4)
  
  params_PC <- 
    c(lambda = 370000, # Replenishment rate of RBC (#SimulatedTimeSeries.R)
      K = 19968254, # Carrying capacity of RBC population in the absence of mortality
      pmax =   pmax_val , # Rate of infection (From American Naturalist- Greischar et al. 2014)
      muR = 0.025, # Daily mortality rate of red blood cells
      muI = 0.025, # Daily mortality rate of infected red blood cells
      c = C_V_opt, # Transmission investment (THE VARYING FACTOR)
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
  
  return(data.frame(out_DDE[, c("time", "R", "G")], B_V = B_V, C_V = C_V_opt))
}

  
