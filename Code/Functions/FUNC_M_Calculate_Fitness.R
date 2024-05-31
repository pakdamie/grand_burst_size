###@ PrI_PC
### Calculates the fitness function (Daily transmission) of 
### Plasmodium chabaudi based on gametocyte numbers
### Input: Gametocyte density (singular or value vector)
### Output: Transmission probability of transmitting to a mosquito

PrI_PC <- function(G) {
  exp(-12.69 + 3.6 * log10(G)) / (1 + exp(-12.69 + 3.6 * log10(G)))
}

###@ PrI_PF
### Calculates the fitness function (Daily transmission) of 
### Plasmodium falciparum based on gametocyte numbers
### Input: Gametocyte density (singular or value vector)
### Output: Transmission probability of transmitting to a mosquito
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

###@Gametocyte Fitness
### A more generalizable function that you can choose which fitness functions
### to use.
### Input: data_frame_G - a dataframe that must contain "time" and "G"
###        species - "PC" (Plasmodium chabaudi) or "PF" (Plasmoidum falciparum)

### Output: The maximum cumulative transmission potential                #
Gametocyte_Fitness <- function(data_frame_G, species) {
  
  gam_dat <- data.frame(
    time = data_frame_G["time"],
    gam = data_frame_G[, "G"]
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
