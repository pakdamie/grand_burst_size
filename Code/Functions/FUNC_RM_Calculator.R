### @RM_Calculator
### This is the function for calculating the time-varying RM
### from the model simulation

### Input: Species ("PC" (Plasmoidum chabaudi) or "PF" (Plasmodium falciparum))
### x_list, a list of model_outputs from the simulation

### Output: A single value of RM. Initial RM must be greater than this
### value for the infection to establish


### Calculate the RM
RM_Calculator <- function(species, x_list) {
  if (!(species %in% c("PC", "PF"))) {
    stop("Invalid input, either input `PC` for P. chabaudi or `PF`
    for P.falciparum")
  }

  ### Time delay
  time_delayer <- switch(species,
    "PC" = ((100 * (1))^100) / ((100 * (1) + 0.025))^100,
    "PF" = ((10 * (1 / 2))^10) / ((10 * (1 / 2) + (1 / 120)))^10
  )

  ### Maximum invasion rate
  p_val <- switch(species,
    "PC" = 4.0e-6,
    "PF" = 8.35e-6
  )

  ### Merozoite mortality rate
  mu_M <- switch(species,
    "PC" = 48,
    "PF" = 200
  )

  ### Burst size
  unique_B_V <- unique(x_list$B_V)

  if (length(unique_BV) > 1) {
    stop("Ensure that each list element is unique!")
  }

  ### Transmission investment
  unique_C_V <- unique(x_list$C_V)

  if (length(unique_C_V) > 1) {
    stop("Ensure that each list element is unique!")
  }

  ### The rate is then calculated here
  rate <- time_delayer * (1 - unique_C_V) * unique_B_V *
    ((x_list[, "R"] * p_val) / ((p_val * x_list[, "R"]) + mu_M))

  return(rate)
}


### @RM_Calculator_Criss_Cross
### This is the function for calculating the time-varying RM
### from the model simulation

### Input: Species ("PC" (Plasmoidum chabaudi) or "PF" (Plasmodium falciparum))
### x_list, a list of model_outputs from the simulation
###      variable_interest: pmax (maximum invasion rate), alpha1 (asexual
###      cycle duration), alpha2 (sexual cycle), muM (merozoite mortality),
###      muG (gametocyte mortality)

### Output: A single value of RM. Initial RM must be greater than this
### value for the infection to establish

RM_Calculator_Criss_Cross <- function(species, x_list, variable_interest) {
  if (!(species %in% c("PC", "PF"))) {
    stop("Invalid input, either input `PC` for P. chabaudi or `PF`
    for P.falciparum")
  }

  if (!(variable_interest %in% c("pmax", "alpha1", "alpha2", "muM", "muG"))) {
    stop("Invalid input, either choose `pmax`, `alpha1`, `alpha2`,`muM`,`muG`")
  }

  if (species == "PC") {
    pmax_val <- ifelse(variable_interest != "pmax", 4.0e-6, 8.35e-6)
    alpha1_val <- ifelse(variable_interest != "alpha1", 1, 1 / 2)
    muM_val <- ifelse(variable_interest != "muM", 48, 200)
    PC_Time_Delayer <- ((100 * (alpha1_val))^100) / ((100 * (alpha1_val) + 0.025))^100

    unique_B_V <- unique(x_list$B_V)
    unique_C_V <- unique(x_list$C_V)

    rate <- PC_Time_Delayer * (1 - unique_C_V) * unique_B_V *
      ((x_list[, "R"] * pmax_val) / ((pmax_val * x_list[, "R"]) + muM_val))
  } else if (species == "PF") {
    pmax_val <- ifelse(variable_interest != "pmax", 8.35e-6, 4.0e-6)
    alpha1_val <- ifelse(variable_interest != "alpha1", 1 / 2, 1)
    muM_val <- ifelse(variable_interest != "muM", 200, 48)
    PF_Time_Delayer <- ((10 * (alpha1_val))^10) / ((10 * (alpha1_val) + (1 / 120)))^10

    ### Burst size
    unique_B_V <- unique(x_list$B_V)

    if (length(unique_BV) > 1) {
      stop("Ensure that each list element is unique!")
    }

    ### Transmission investment
    unique_C_V <- unique(x_list$C_V)

    if (length(unique_C_V) > 1) {
      stop("Ensure that each list element is unique!")
    }

    rate <- PF_Time_Delayer * (1 - unique_C_V) * unique_B_V *
      ((x_list[, "R"] * pmax_val) / ((pmax_val * x_list[, "R"]) + muM_val))
  }

  return(rate)
}
