#' Finds the optimal burst size depending on the variable(s) of interest
#'
#'For a given data.frame, gives you the optimal burst size
#'
#' @param fitness_frame 
#' @param variable_interest 
#'
#' @return
#' @export
#'
#' @examples
#' 
Find_OptimalBV_Group <- function(fitness_frame, variable_interest){
        
  ###Subset the data.frame to only account for successful infections        
  fitness_frame <- subset(fitness_frame, fitness_frame$status == 'success')
  
  if(variable_interest != "combo"){ #if NOT looking at mu_M/pmax at the same
  splitted_group <- split(fitness_frame, fitness_frame[,variable_interest])
  
  } else { #if looking at mu_M/pmax at the same time
  splitted_group <- split(fitness_frame, fitness_frame[,c("mu_M", "p_val")])
 
  }
 
  #for each list element, figure out the maximum fitness AND the optimal
  #burst size from it.
  optimal_df <- (do.call(rbind,
          lapply(splitted_group, function (x) x[which.max(x$end_fitness),])))
  
  return(optimal_df)
}
