##This is the function that connects the host and the parasite
###data together

Parasite_Data_Collector <- function(parasite_host_data, parasite_data) {
  
  ###Ensure that we're getting rid of all white-spaces from entry error
  parasite_host_data$Parasite <- str_replace_all(parasite_host_data$Parasite, " ", "")
  parasite_data$Plasmodium.species  <- str_replace_all(parasite_data$Plasmodium.species , " ", "")
  
  ###now we can split apart all the Plasmodium species 
  para_species <- (str_split(parasite_host_data$Parasite,";"))
  

  para_data_holder <- NULL
  ###For each host species; look at each Plasmodium species 
  
  for (k in seq(1, length(para_species))){
    
      
  dat_interest <- subset(parasite_data, 
                         parasite_data$Plasmodium.species %in%  
                         c(unlist(para_species[[k]])))
  ###Note the length between the parasites found for the host and 
  ### the corresponding parasite entry will be different - for one thing
  ### the parasites were excluded from further analysis 
  
  dat_interest_sub <- dat_interest[,c( "Subgenus",
        "Plasmodium.species",
        "Average", 
        "Lower", 
        "Upper",
        "Duration"
      )]

  if (nrow(dat_interest_sub) != 0){

   para_data_holder[[k]] <- cbind(dat_interest_sub, parasite_host_data[k,])
  }
    
  }

  para_data_f <- do.call(rbind, para_data_holder)

  return( para_data_f)
}


