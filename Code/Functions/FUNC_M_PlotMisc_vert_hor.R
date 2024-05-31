###This function is to draw out the boundaries of
###mortality and establishment 

library(reshape2)

grapher_mortality_boundary <- function(x) {
  tmp_df <- data.frame(
    B_V = x$B_V,
    C_V = x$C_V,
    Status = x$status
  )

  tmp_df$Status_Num <- ifelse(tmp_df$Status == "mort", 1, 0)

  tmp_mat_cast_mort <- acast(tmp_df,
    C_V ~ B_V,
    value.var = c("Status_Num")
  )

  vertical_df_mort <- NULL
  for (n in seq(1, nrow(tmp_mat_cast_mort))) {
    row_numbers <- as.numeric(rownames(tmp_mat_cast_mort)[n])

    tmp_vec_mort <- tmp_mat_cast_mort[n, 2:ncol(tmp_mat_cast_mort)] -
      tmp_mat_cast_mort[n, 1:ncol(tmp_mat_cast_mort) - 1]

    vertical_mort <- as.numeric(names(tmp_vec_mort[tmp_vec_mort == 1]))

    vertical_F_mort <- ifelse(length(vertical_mort) == 0, NA, vertical_mort)

    row_F_mort <- ifelse(length(vertical_mort) == 0, NA, row_numbers)

    vertical_df_mort[[n]] <- data.frame(
      x = vertical_F_mort - 0.25,
      xend = vertical_F_mort - 0.25,
      y = row_F_mort - 0.005,
      yend = row_F_mort + 0.005
    )
  }

  vert_seg_mort <- na.omit(do.call(rbind, vertical_df_mort))

  if (nrow(vert_seg_mort) != 0) {
    vert_seg_mort$id <- "mort"
  } else {
    vert_seg_mort <- NA
  }

  ####################################
  ### Horizontal lines for mortality###
  ####################################
  horizontal_df_mort <- NULL
  for (n in seq(1, ncol(tmp_mat_cast_mort))) {
    col_mort <- as.numeric(colnames(tmp_mat_cast_mort)[n])

    tmp_vec_mort <- tmp_mat_cast_mort[2:nrow(tmp_mat_cast_mort), n] -
      tmp_mat_cast_mort[1:nrow(tmp_mat_cast_mort) - 1, n]

    horizontal_mort <- as.numeric(names(tmp_vec_mort[tmp_vec_mort == -1]))

    horizontal_F_mort <- ifelse(length(horizontal_mort) == 0, NA,
      horizontal_mort
    )

    col_F_mort <- ifelse(length(horizontal_mort) == 0, NA, col_mort)

    horizontal_df_mort[[n]] <- data.frame(
      x = col_F_mort - 0.25,
      xend = col_F_mort + 0.25,
      y = horizontal_F_mort - 0.005,
      yend = horizontal_F_mort - 0.005
    )
  }
  hor_seg_mort <- na.omit(do.call(rbind, horizontal_df_mort))

  if (nrow(hor_seg_mort) != 0) {
    hor_seg_mort$id <- "mort"
  } else {
    hor_seg_mort <- NA
  }

  ################################
  ### Does not establish infection#
  ################################

  tmp_df <- data.frame(
    B_V = as.factor(x$B_V),
    C_V = as.factor(x$C_V),
    Status = x$status
  )

  tmp_df$Status_Num_fail <- ifelse(tmp_df$Status == "Fail", 0, 1)

  tmp_mat_cast_fail <- acast(tmp_df,
    C_V ~ B_V,
    value.var = c("Status_Num_fail")
  )


  vertical_df_fail <- NULL
  for (n in seq(1, nrow(tmp_mat_cast_fail))) {
    row <- as.numeric(rownames(tmp_mat_cast_fail)[n])

    tmp_vec <- tmp_mat_cast_fail[n, 2:ncol(tmp_mat_cast_fail)] -
      tmp_mat_cast_fail[n, 1:ncol(tmp_mat_cast_fail) - 1]

    vertical <- as.numeric(names(tmp_vec[tmp_vec == 1]))

    vertical_F <- ifelse(length(vertical) == 0, NA, vertical)
    row_F <- ifelse(length(vertical) == 0, NA, row)

    vertical_df_fail[[n]] <- data.frame(
      x = vertical_F - 0.25,
      xend = vertical_F - 0.25,
      y = row_F - 0.005,
      yend = row_F + 0.005
    )
  }

  vert_seg_fail <- na.omit(do.call(rbind, vertical_df_fail))
  vert_seg_fail$id <- "fail"

  horizontal_df_fail <- NULL

  for (n in seq(1, ncol(tmp_mat_cast_fail))) {
    col <- as.numeric(colnames(tmp_mat_cast_fail)[n])

    tmp_vec <- tmp_mat_cast_fail[2:nrow(tmp_mat_cast_fail), n] -
      tmp_mat_cast_fail[1:nrow(tmp_mat_cast_fail) - 1, n]

    horizontal <- as.numeric(names(tmp_vec[tmp_vec == -1]))

    horizontal_F <- ifelse(length(horizontal) == 0, NA, horizontal)
    col_F <- ifelse(length(horizontal) == 0, NA, col)

    horizontal_df_fail[[n]] <- data.frame(
      x = col_F - 0.25,
      xend = col_F + 0.25,
      y = horizontal_F - 0.005,
      yend = horizontal_F - 0.005
    )
  }
  hor_seg_fail <- na.omit(do.call(rbind, horizontal_df_fail))
  hor_seg_fail$id <- "fail"

  full_df_fail_mort <- rbind.data.frame(
    vert_seg_mort,
    hor_seg_mort,
    vert_seg_fail,
    hor_seg_fail
  )

  return(full_df_fail_mort)
}
