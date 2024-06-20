
###@ Find_OptimalBV_Group 
### Finds the optimal burst size depending on the variable of interest-
### this is used to figure out what the optimal burst size is for the 
### data.frame
### 

Find_OptimalBV_Group <- function(fitness_frame, variable_interest){
  
  fitness_frame <- subset(fitness_frame, fitness_frame$status == 'success')
  
  splitted_group <- split(fitness_frame, fitness_frame[,variable_interest])
  
  
  
  
  optimal_df <- (do.call(rbind,
          lapply(splitted_group, function (x) x[which.max(x$end_fitness),])))
  
  return(optimal_df)
}
