### @Calculate_Initial_RM
### This is the function for calculating the initial RM
### and allows user to figure out what parameter combinations
### will not lead to establishing of infection (thus avoiding
### running this)

### Input: Species ("PC" (Plasmodium chabaudi) or "PF" (Plasmodium falciparum)),
### variable_interest = pmax (invasion rate) or mu_M (merozoite mortality)
### variable_interest_value = the value to be used for the variable interest

### Output: A single value of RM. Initial RM must be greater than this
### value for the infection to establish

Calculate_Initial_RM <- function(species, p_val, mu_M, R_modifier, alpha_1) {
  
  if (!(species %in% c("PC", "PF"))) {
    stop("Invalid input, either input `PC` for P. chabaudi or `PF`
    for P.falciparum")
  }

  initial_RM_modifier <- 1.5 # The threshold for establishment.
  
  alpha_mod <- switch(species,
                     "PC" = alpha_1 * 1,
                     "PF" = alpha_1 * 1/2)
  
  p_mod <- switch(species,
                  "PC" = p_val * 4.0e-6,
                  "PF" = p_val * 8.35e-6)
  
  mu_mod <- switch(species,
                   "PC" = mu_M * 48,
                   "PF" = mu_M* 200)
  R_mod <- switch(species,
                  "PC" = R_modifier *  8500000,
                  "PF" = R_modifier * 5e6)
  
  ### We need to include the time delayer (check methods)
  time_delayer <- switch(species,
    "PC" = ((100 * ( alpha_mod ) + (1 / 40))^100) / (100 * ( alpha_mod ))^100,
    "PF" = ((10 * ( alpha_mod) + (1 / 120))^10) / (10 * ( alpha_mod ))^10
  )


 
  
  

  RM_limit_1 <- initial_RM_modifier * time_delayer * ((p_mod * R_mod) + mu_mod) / (p_mod * R_mod)

  return(RM_limit_1)
}

### Testing function:
### Calculate_Initial_RM ("PC", 0.05, 0.05)
### Calculate_Initial_RM ("PC", 0.5, 0.05)
### Calculate_Initial_RM ("PC") - should not work
