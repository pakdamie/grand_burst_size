
library(here)
library(ggplot2)
library(grid)
library(stringr)
host_dat <- read.csv(here("Data","MALARIA_PAK_HOSTS.csv"))

Host_List <- NULL

for (species in seq(1,nrow(host_dat))){
###Go through each row (looking at each host) and extract the malaria species
    parasite_species <- str_split_1(host_dat[species,]$P.Species, ";")
    ###
    times_to_replicate <- length(parasite_species)
    ### Recreate the data.frame 
    Host_List[[species]] <- cbind.data.frame(
                                     do.call("rbind", replicate(
                                     times_to_replicate,  
                                     host_dat[species,-4], 
                                     simplify = FALSE)),
                                     P.Species = parasite_species)
}

Host_List_F <- do.call(rbind, Host_List)

###Try Mammal
Host_List_F_Mammal <- subset(Host_List_F, Host_List_F$Order == 'Mammal')
Host_List_F_Mammal$Presence <- 1


Mammal_Expanded_Grid <- 
            expand.grid(H.Species = unique(Host_List_F_Mammal$H.Species),
            P.Species =  unique(Host_List_F_Mammal$P.Species))

Full_Mammal_Expanded_Grid <- left_join(Mammal_Expanded_Grid, 
                                       Host_List_F_Mammal,
                                       by = c("H.Species", "P.Species"))

###0 indices 
zero_indices_mammal<- is.na(Full_Mammal_Expanded_Grid$Presence)==TRUE

Full_Mammal_Expanded_Grid$Presence[zero_indices_mammal] <- 0

ggplot(Full_Mammal_Expanded_Grid, 
      aes(x = H.Species, y = P.Species, fill = as.factor(Presence),
          color = as.factor(Presence)))+
      geom_tile()+
      scale_color_manual(values=c("1" = "black", "0" = 'darkgrey'))+
      scale_fill_manual(values=c("1" = "#FF5632", "0" = '#d8d4d4'))+
      theme(axis.text.x = element_text(
                          angle = 90, 
                          vjust = 0.5, 
                          hjust=1)) + 
      xlab("Host species")+ 
      ylab("Plasmoidum species")
      

###Try Reptile
Host_List_F_Reptile <- subset(Host_List_F, Host_List_F$Order == 'Reptile')
Host_List_F_Reptile$Presence <- 1


Reptile_Expanded_Grid <- 
  expand.grid(H.Species = unique(Host_List_F_Reptile$H.Species),
              P.Species =  unique(Host_List_F_Reptile$P.Species))

Full_Reptile_Expanded_Grid <- left_join(Reptile_Expanded_Grid, 
                                       Host_List_F_Reptile,
                                       by = c("H.Species", "P.Species"))

###0 indices 
zero_indices_Reptile<- is.na(Full_Reptile_Expanded_Grid$Presence)==TRUE

Full_Reptile_Expanded_Grid$Presence[zero_indices_Reptile] <- 0

ggplot(Full_Reptile_Expanded_Grid, 
       aes(x = H.Species, y = P.Species, fill = as.factor(Presence),
           color = as.factor(Presence)))+
  geom_tile()+
  scale_color_manual(values=c("1" = "black", "0" = 'darkgrey'))+
  scale_fill_manual(values=c("1" = "#FF5632", "0" = '#d8d4d4'))+
  theme(axis.text.x = element_text(
    angle = 90, 
    vjust = 0.5, 
    hjust=1)) + 
  xlab("Host species")+ 
  ylab("Plasmoidum species")

