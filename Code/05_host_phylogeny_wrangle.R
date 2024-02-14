### Cleaning up the phylogeny for the mammal, avian, and reptile
### hosts as well as including the pertinent data. This script
### is specifically for cleaning up the host phylogeny data
### without thinking about the parasites for modularity 


library(here)
source(here("Code", "Functions", "Package_Loader.R"))
source(here("Code","Functions","Merger_ParasiteTrait_HostPhylo.R"))
source(here("Code","Functions","util_remove_underscore.R"))
source(here("Code", "Functions", "Identifier_Burst_Size_Order.R"))
source(here("Code", "Functions","util_remove_underscore.R"))

###Host and Parasite data
Malaria_Host_Dat <- read.csv(here("Data", "MALARIA_PAK_HOSTS.csv"))
Malaria_Parasite_Dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

###We need to get rid of circumflexum_1 get rid of (which is row 50) 
###as it is a duplicate entry (two different asexual development time
###was found)
Malaria_Parasite_Dat[Malaria_Parasite_Dat$Plasmodium.species %in%  c('circumflexum_2','lophurae_2'),] <- NA
Malaria_Parasite_Dat[na.omit(Malaria_Parasite_Dat$Plasmodium.species == 'circumflexum_1'),]$Plasmodium.species <- "circumflexum"
Malaria_Parasite_Dat[na.omit(Malaria_Parasite_Dat$Plasmodium.species == 'lophurae_1'),]$Plasmodium.species <- "lophurae"

Malaria_Parasite_Dat <- subset(Malaria_Parasite_Dat,
                               !(Malaria_Parasite_Dat$Include == "No"))

###REPTILE
REP_Host_Dat <- subset(Malaria_Host_Dat,
                          Malaria_Host_Dat$Group == 'reptile')
###AVIAN
AVE_Host_Dat <- subset(Malaria_Host_Dat,
                         Malaria_Host_Dat$Group == 'avian')
###MAMMAL
MAM_Host_Dat <- subset(Malaria_Host_Dat,
                         Malaria_Host_Dat$Group == 'mammal')

###For easier matching, I'm adding the underscore to my
###host data because the phylogenetic trees species' name have
###underscores

REP_Host_Dat$Species<-  add_underscore(REP_Host_Dat$Species) #reptile
AVE_Host_Dat$Species <- add_underscore(AVE_Host_Dat$Species) #avian
AVE_Host_Dat$Species[94] <- "Pulsatrix_koeniswaldiana" 
MAM_Host_Dat$Species <- add_underscore(MAM_Host_Dat$Species)

parasite_data <- Malaria_Parasite_Dat
###Note that I already subsetted the information necessary for the 
###mammalian, avian , and reptilian
AVE_Merged <- Parasite_Data_Collector(AVE_Host_Dat,
                                       Malaria_Parasite_Dat)

MAM_Merged <- Parasite_Data_Collector(MAM_Host_Dat,
                                       Malaria_Parasite_Dat)

REP_Merged <- Parasite_Data_Collector(REP_Host_Dat,
                                      Malaria_Parasite_Dat)

###Combine

All_Order_Merged <- rbind(AVE_Merged,MAM_Merged,REP_Merged )


