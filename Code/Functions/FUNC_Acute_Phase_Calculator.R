### @Finder_RM
### This is the function for calculating the initial RM
### and allows user to figure out what parameter combinations
### will not lead to establishing of infection (thus avoiding
### running this)

### Input:
### .       xlist: Model output from the main simulation
###       species: ("PC" (Plasmoidum chabaudi) or "PF" (Plasmodium falciparum))
###       criss_cross: "YES" or "NO", permute the parameters for PF and PC
###      variable_interest: pmax (maximum invasion rate), alpha1 (asexual
###      cycle duration), alpha2 (sexual cycle), muM (merozoite mortality),
###      muG (gametocyte mortality)

### Output: A data.frame containing the end of the acute phase
Finder_RM <- function(x_list, species, criss_cross, variable_interest) {
  if (!(species %in% c("PC", "PF"))) {
    stop("Invalid input, either input `PC` for P. chabaudi or `PF`
    for P.falciparum")
  }

  if (!(variable_interest %in% c("pmax", "alpha1", "alpha2", "muM", "muG"))) {
    stop("Invalid input, either choose `pmax`, `alpha1`, `alpha2`,`muM`,`muG`")
  }

  ### If it's NA that means that it never kills the host,
  ### If it's a numeric value, that means it kills the host.
  infection_length <- unique(x_list$infection_length)

  ### If the infection has an actual length (aka NA is false)
  if (is.na(infection_length) == FALSE) {
    ### Look at the gametocyte time series
    ### Calculating the fitness
    G_TS <- data.frame(
      time = x_list[, "time"],
      G = x_list[, "G"]
    )

    # Prevent negative gametocytes if there are any
    G_TS$G[G_TS$G < 0] <- 0

    ### Let's approximate the gametocyte time as a function
    G_TS_Function <- approxfun(G_TS)

    ### Run the new function to the point of the infection length
    ### with time steps of 1/10
    truncate_G_TS <- G_TS_Function(seq(0, infection_length, 1 / 10))

    ### Choose the transmission probability calculator depending on
    ### the species
    fitness_func <- switch(species,
      "PC" = PrI_PC,
      "PF" = PrI_PF
    )

    ### The maximum fitness at the time of host death
    end_fitness_mort <- sum(fitness_func(truncate_G_TS) * 1 / 10)

    ### This is now a data.frame that has the end-fitness and the infection length

    df <- data.frame(
      endtime = infection_length,
      up_down = "up",
      end_fitness = end_fitness_mort,
      status = "mort" ### This parameter combination leads to death.
    )
  } else {
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

    ### If Criss_crossing - use the RM_Calculator_Criss_Cross
    ### If not - then use the standard.
    criss_cross_RM <- switch(criss_cross,
      "NO" = RM_Calculator,
      "YES" = RM_Calculator_Criss_Cross
    )

    ### These create a data.frame with the RM rate (ala 'rate')

    if (criss_cross == "YES") {
      RM_time_df <- cbind.data.frame(
        time = x_list[, "time"],
        rate = criss_cross_RM(species, x_list, variable_interest),
        B_V = unique_B_V,
        C_V = unique_C_V
      )
    } else if (criss_cross == "NO") {
      RM_time_df <- cbind.data.frame(
        time = x_list[, "time"],
        rate = criss_cross_RM(species, x_list),
        B_V = unique_B_V,
        C_V = unique_C_V
      )
    }

    ### Find when there lowest RM is (should be negative)

    min_RM <- RM_time_df[which.min(RM_time_df$rate), ]

    ### The end of the acute phase is then

    ### Look for time after lowest RM peak (post peak)
    ### and when the RM is equal or greater than 1

    end_time <- subset(
      RM_time_df,
      RM_time_df$time >= min_RM$time &
        RM_time_df$rate >= 1
    )[1, "time"]


    ### This is now a data.frame that has the end-fitness and the infection length

    df <- data.frame(
      endtime = end_time,
      up_down = "up",
      end_fitness = NA,
      status = "success"
    )
  }

  return(df)
}
