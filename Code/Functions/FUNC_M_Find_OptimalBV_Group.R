

Find_OptimalBV_Group <- function(fitness_frame, variable_interest){
  
  splitted_group <- split(fitness_frame, fitness_frame[,variable_interest])
  
  optimal_df <- (do.call(rbind,
          lapply(splitted_group, function (x) x[which.max(x$end_fitness),])))
  
  return(optimal_df)
}
