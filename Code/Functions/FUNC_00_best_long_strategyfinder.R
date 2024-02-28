###Best strategy and longest strategy finders

Best_Strategy_Finder <- function(x){
  optimum_strategy <- x[which.max(x$end_fitness),]
  return(optimum_strategy )
}

Longest_Finder <- function(x){
  longest_strategy <- x[which.max(x$endtime),]
  
  return(longest_strategy  )
}