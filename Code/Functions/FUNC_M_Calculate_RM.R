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
  
  unique_p_val <- unique(x_list$p_val)
  if (length( unique_p_val ) > 1) {
    stop("Ensure that each list element is unique!")
  }
  
  unique_mu_M_val <- unique(x_list$mu_M)
  if (length(unique_mu_M_val) > 1) {
    stop("Ensure that each list element is unique!")
  }

  ### Burst size
  unique_B_V <- unique(x_list$B_V)

  if (length(unique_B_V) > 1) {
    stop("Ensure that each list element is unique!")
  }

  ### Transmission investment
  unique_C_V <- unique(x_list$C_V)

  if (length(unique_C_V) > 1) {
    stop("Ensure that each list element is unique!")
  }

  ### The rate is then calculated here
  rate <- time_delayer * (1 - unique_C_V) * unique_B_V *
    ((x_list[, "R"] * unique_p_val) / ((unique_p_val * x_list[, "R"]) + unique_mu_M_val ))

  return(rate)
}
