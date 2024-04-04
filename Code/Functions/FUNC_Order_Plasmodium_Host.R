###This lets me order the Plasmodium species based on the 
###host phylogeny 

Ordering_PlasmodiumSP_Burst <- function(malaria_dat, tip_names){
    
    Order_list <- NULL

    for (host_species in seq(1,nrow(tip_names))){
        host_interest <- tip_names$label[[host_species]]
        
        plas_interests <- subset(malaria_dat,
                                 malaria_dat$Type.Host == 
                                   host_interest)[,c("Plasmodium.species",
                                                     "Type.Host",
                                                     "Family",
                                                     "Subgenus",
                                                     "Average", 
                                                     "Lower", 
                                                     "Upper")]
        
        Ordered_Ave_Species <- plas_interests[order(plas_interests$Subgenus, 
                               plas_interests$Plasmodium.species),]
        
        Order_list[[host_species]] <- Ordered_Ave_Species 
        
    }
    Order_DF <- do.call(rbind, Order_list)
    
    Order_DF$Indivdiual_Number <- seq(1,nrow(Order_DF))
    
    return(Order_DF)
}
