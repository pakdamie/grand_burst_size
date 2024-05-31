###Best strategy and longest strategy finder 
###for when you give the fitness data.frame as an input

###Greatest end fitness
Best_Strategy_Finder <- function(x){
  optimum_strategy <- x[which.max(x$end_fitness),]
  return(optimum_strategy )
}

###Longest acute phase
Longest_Finder <- function(x){
  longest_strategy <- x[which.max(x$endtime),]
  
  return(longest_strategy  )
}