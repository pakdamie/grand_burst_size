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

Calculate_Initial_RM <- function(species, p_val, mu_M) {
  
  if (!(species %in% c("PC", "PF"))) {
    stop("Invalid input, either input `PC` for P. chabaudi or `PF`
    for P.falciparum")
  }

  initial_RM_modifier <- 1.5 # The threshold for establishment.

  ### We need to include the time delayer (check methods)
  time_delayer <- switch(species,
    "PC" = ((100 * (1 / 1) + (1 / 40))^100) / (100 * (1 / 1))^100,
    "PF" = ((10 * (1 / 2) + (1 / 120))^10) / (10 * (1 / 2))^10
  )

  ### Initial RBCs - this remain unchanged!
  R_val <- switch(species,
    "PC" = 8500000,
    "PF" = 5e6
  )

  RM_limit_1 <- initial_RM_modifier * time_delayer * ((p_val * R_val) + mu_M) / (p_val * R_val)

  return(RM_limit_1)
}

### Testing function:
### Calculate_Initial_RM ("PC", 0.05, 0.05)
### Calculate_Initial_RM ("PC", 0.5, 0.05)
### Calculate_Initial_RM ("PC") - should not work
