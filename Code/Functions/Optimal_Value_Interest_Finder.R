Optimal_burst_size_finder <- function(x_dataframe, species){
  
  if('variable_interest_1' %in% colnames(x_dataframe)){
  
  successful_DF <- subset(x_dataframe, x_dataframe$status == 'success')
  
  splitted_success_DF <- split(successful_DF,  list(successful_DF$variable_interest_1,
                                                    successful_DF$variable_interest_2))
  
  optimal_DF <- do.call(rbind,lapply(splitted_success_DF , function(x) x[which.max(x$end_fitness),]))
  optimal_DF$species <- species
  
  }else{
    successful_DF <- subset(x_dataframe, x_dataframe$status == 'success')
    
    splitted_success_DF <- split(successful_DF,  list(successful_DF$variable_interest))
    
    optimal_DF <- do.call(rbind,lapply(splitted_success_DF , function(x) x[which.max(x$end_fitness),]))
    optimal_DF$species <- species  
    
  }
  
  return(optimal_DF)

  
}
