###This script is to clean the data to make the large
###burst size figure 
library(here)
source(here("Code","Functions","FUNC_Order_Plasmodium_Host.R"))
source(here("Code","Data_Analysis" ,"00_host_phylogeny_tree.R"))
mal_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

###We need to get rid of circumflexum_1 get rid of (which is row 50) 
###as it is a duplicate entry (two different asexual development time
###was found)
mal_dat[mal_dat$Plasmodium.species %in%  c('circumflexum_2','lophurae_2'),] <- NA

###No longer manually doing it with my own ordering, 
###but with the phylo tree

#Include = No is important! It means that the Plasmodium 
#is not included ()

mal_avian <- subset(mal_dat, mal_dat$OrderHost == 'avian' &
                      mal_dat$Include != "No")
mal_mammal<- subset(mal_dat, mal_dat$OrderHost == 'mammal' &
                      mal_dat$Include != "No")
mal_reptile <- subset(mal_dat, mal_dat$OrderHost == 'reptile' &
                      mal_dat$Include != "No")

###Avian and consensus_AVE_Tree - look at the species not found in the tree
mal_avian$Type.Host <- sub(" ", "_", mal_avian$Type.Host)
#subset(mal_avian, !(mal_avian$Type.Host %in% consensus_AVE_Tree$tip.label))$Type.Host
tip_name_ave <- ggtree(keep.tip(consensus_AVE_Tree, 
                                mal_avian$Type.Host))[["data"]][,c('label','y')]

tip_name_ave <- tip_name_ave[order(tip_name_ave$y),]

###Gets rid of internal nodes
tip_name_ave <- subset(tip_name_ave, (tip_name_ave$y)%%1== 0 &
                                      tip_name_ave$label != 1)  
Ordered_Ave_Species <- Ordering_PlasmodiumSP_Burst(mal_avian,tip_name_ave)
Ordered_Ave_Species$Plasmodium.species <- factor(Ordered_Ave_Species$Plasmodium.species,
                                                  levels = Ordered_Ave_Species$Plasmodium.species )


###Mammal and MAM_Tree - look at the species not found in the tree
mal_mammal$Type.Host <- sub(" ", "_", mal_mammal$Type.Host)

#subset(mal_mammal, !(mal_mammal$Type.Host %in% consensus_MAM_Tree$tip.label))$Type.Host

tip_name_mam <- ggtree(keep.tip(consensus_MAM_Tree, 
                                mal_mammal$Type.Host))[["data"]][,c('label','y')]
tip_name_mam <- tip_name_mam[order(tip_name_mam$y),]
tip_name_mam <- subset(tip_name_mam, tip_name_mam$y%%1== 0 &
                                     tip_name_mam$label != 1)

Ordered_Mam_Species <- Ordering_PlasmodiumSP_Burst(mal_mammal,tip_name_mam )

Ordered_Mam_Species$Plasmodium.species <- factor(Ordered_Mam_Species $Plasmodium.species,
                                                 levels = Ordered_Mam_Species$Plasmodium.species )



###Mammal and MAM_Tree - look at the species not found in the tree
mal_mammal$Type.Host <- sub(" ", "_", mal_mammal$Type.Host)

#subset(mal_mammal, !(mal_mammal$Type.Host %in% consensus_MAM_Tree$tip.label))$Type.Host

tip_name_mam <- ggtree(keep.tip(consensus_MAM_Tree, 
                                mal_mammal$Type.Host))[["data"]][,c('label','y')]
tip_name_mam <- tip_name_mam[order(tip_name_mam$y),]
tip_name_mam <- subset(tip_name_mam, tip_name_mam$y%%1== 0 &
                         tip_name_mam$label != 1)

Ordered_Mam_Species <- Ordering_PlasmodiumSP_Burst(mal_mammal,tip_name_mam)

Ordered_Mam_Species$Plasmodium.species <- factor(Ordered_Mam_Species $Plasmodium.species,
                                                 levels = Ordered_Mam_Species$Plasmodium.species )


###Reptile and REP_Tree - look at the species not found in the tree
mal_reptile$Type.Host <- sub(" ", "_", mal_reptile$Type.Host)

#subset(mal_reptile, !(mal_reptile$Type.Host %in% consensus_MAM_Tree$tip.label))$Type.Host

tip_name_rep <- ggtree(keep.tip(consensus_REP_Tree, 
                                mal_reptile$Type.Host))[["data"]][,c('label','y')]
tip_name_rep <- tip_name_rep[order(tip_name_rep$y),]
tip_name_rep <- subset(tip_name_rep, tip_name_rep$y%%1== 0 &
                         tip_name_rep$label != 1)

Ordered_rep_Species <- Ordering_PlasmodiumSP_Burst(mal_reptile,tip_name_rep )

Ordered_rep_Species$Plasmodium.species <- factor(Ordered_rep_Species $Plasmodium.species,
                                                 levels = Ordered_rep_Species$Plasmodium.species )

mal_reptile_order_1 <- Ordered_rep_Species[1:55,]
mal_reptile_order_2 <- Ordered_rep_Species[56:111,]

