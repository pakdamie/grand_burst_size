
Simulator_PF_Criss_Cross_DOUBLE_TROUBLE <- function(B_V,C_V_opt,initialvalue){
  
  rootfun <- function (t, y, parms) {return(y['R'] - 380000)}
  params_PF<- c(lambda = 2e5, # replenishment rate of RBC (#SimulatedTimeSeries.R)
                K = 6315789, # carrying capacity of RBC population in the absence of mortality (#Simulated Time series)
                pmax =  8.35e-6 , # rate of infection (From American Naturalist- Greischar et al. 2014)
                muR = 1/120, #Daily mortality rate of red blood cells (SimulatedTimeSeries.R)
                muI = 1/120, #Daily mortality rate of infected red blood cells (SimulatedTimeSeries.R)
                c = C_V_opt, # transmission investment (Vary)
                B = B_V, # the burst size (Vary)
                alpha1 = 1, # the rate of development of parasite in iRBC (SimulatedTimeSeries.R)
                alpha2 = 1/7, #the rate of development (SimulatedTimeSeries.R)
                muM =48, # background mortality of the merozoite (SimulatedTimeSeries.R)
                muG =log(2)/2.4, # background mortality of the immature/mature gametocytes (SimulatedTimeSeries.R)
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


RM_Calculator_Criss_Cross_DOUBLE <- function( x_list){
  
    pmax_val <- 8.35e-6 
    alpha1_val <- 1
    muM_val  <- 48
    
    PF_Time_Delayer <- ((10 * alpha1_val + (1 / 120))^10) / (10 *  alpha1_val )^10
    
    unique_B_V <- unique(x_list$B_V)
    unique_C_V <- unique(x_list$C_V)
    
    rate =  PF_Time_Delayer * (1 - unique_C_V) * unique_B_V *
      ((x_list[,"R"] *  pmax_val)/(( pmax_val * x_list[,"R"]) + muM_val))    
    
  
  
  return(rate)
}
