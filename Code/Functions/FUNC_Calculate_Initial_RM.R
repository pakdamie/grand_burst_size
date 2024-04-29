###@Calculate_Initial_RM 
### This is the function for calculating the initial RM
### and allows user to figure out what parameter combinations
### will not lead to establishing of infection (thus avoiding
### running this)

### Input: Species ("PC" (Plasmoidum chabaudi) or "PF" (Plasmodium falciparum))

### Output: A single value of RM. Initial RM must be greater than this
### value for the infection to establish

Calculate_Initial_RM <- function(species) {
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

  ### Invasion rate
  p_val <- switch(species,
    "PC" = 4.0e-6,
    "PF" = 8.35e-6
  )

  ### Merozoite mortality rate
  mu_M <- switch(species,
    "PC" = 48,
    "PF" = 200
  )

  ### Initial RBCs
  R_val <- switch(species,
    "PC" = 8500000,
    "PF" = 5e6
  )

  RM_limit_1 <- initial_RM_modifier * time_delayer * ((p_val * R_val) + mu_M) / (p_val * R_val)

  return(RM_limit_1)
}

###@ Calculate_Initial_RM_CrissCross
### This is the function for calculating the initial RM
### and allows user to figure out what parameter combinations
### will not lead to establishing of infection (thus avoiding
### running this). This is the same as above, but when you're
### permuting P. chabaudi and Plasmoidum falciparum

### Input: Species ("PC" (Plasmodium chabaudi)  or "PF" (Plasmodium falciparum))
###      variable_interest: pmax (maximum invasion rate), alpha1 (asexual
###      cycle duration), alpha2 (sexual cycle), muM (merozoite mortality),
###      muG (gametocyte mortality)

### Output: A single value of RM. Initial RM must be greater than this
### value for the infection to establish

Calculate_Initial_RM_CrissCross <- function(species, variable_interest) {
  if (!(species %in% c("PC", "PF"))) {
    stop("Invalid input, either input `PC` for P. chabaudi or `PF`
    for P.falciparum")
  }

  if (!(variable_interest %in% c("pmax", "alpha1", "alpha2", "muM", "muG"))) {
    stop("Invalid input, either choose `pmax`, `alpha1`, `alpha2`,`muM`,`muG`")
  }

  initial_RM_modifier <- 1.5 # The threshold for establishment.

  if (species == "PC") { # If the species is Plasmodium chabaudi
    R_val <- 8500000
    pmax_val <- ifelse(variable_interest != "pmax", 4.0e-6, 8.35e-6)
    alpha1_val <- ifelse(variable_interest != "alpha1", 1, 1 / 2)
    alpha2_val <- ifelse(variable_interest != "alpha2", 1 / 2, 1 / 7)
    muM_val <- ifelse(variable_interest != "muM", 48, 200)
    muG_val <- ifelse(variable_interest != "muG", 4, log(2) / 2.4)

    ### The time delay will be different if it is chosen.
    time_delayer_CROSSED <- ((100 * alpha1_val + (1 / 40))^100) / (100 * alpha1_val)^100

    RM_limit_1 <- initial_RM_modifier * time_delayer_CROSSED * ((pmax_val * R_val) + muM_val) / (pmax_val * R_val)
  } else if (species == "PF") { # If the species is Plasmodium falciparum
    R_val <- 5e6
    pmax_val <- ifelse(variable_interest != "pmax", 8.35e-6, 4.0e-6)
    alpha1_val <- ifelse(variable_interest != "alpha1", 1 / 2, 1)
    alpha2_val <- ifelse(variable_interest != "alpha2", 1 / 7, 1 / 2)
    muM_val <- ifelse(variable_interest != "muM", 200, 48)
    muG_val <- ifelse(variable_interest != "muG", log(2) / 2.4, 4)

    time_delayer_CROSSED <- ((10 * alpha1_val + (1 / 120))^10) / (10 * alpha1_val)^10

    RM_limit_1 <- initial_RM_modifier * time_delayer_CROSSED * ((pmax_val * R_val) + muM_val) / (pmax_val * R_val)
  }
  return(RM_limit_1)
}
