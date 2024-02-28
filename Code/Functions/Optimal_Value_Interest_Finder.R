Optimal_burst_size_finder <- function(x_dataframe, species){
  
  successful_DF <- subset(x_dataframe, x_dataframe$status == 'success')
  
  splitted_success_DF <- split(successful_DF,  successful_DF$variable_interest)
  
  optimal_DF <- do.call(rbind,lapply(splitted_success_DF , function(x) x[which.max(x$end_fitness),]))
  optimal_DF$species <- species
  
  optimal_DF["alpha1",]$B_V <- sqrt(  optimal_DF["alpha1",]$B_V )
  
  return(optimal_DF)
  
  
  
}
