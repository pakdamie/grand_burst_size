### Cleaning up the phylogeny for the mammal, avian, and reptile
### hosts as well as including the pertinent data.
### Cleaning script.
library(here)
source(here("Code", "Functions", "Package_Loader.R"))
source(here("Code","Functions","Merger_ParasiteTrait_HostPhylo.R"))

############################
###Host and Parasite data ##
############################
Malaria_Host_Dat <- read.csv(here("Data", "MALARIA_PAK_HOSTS.csv"))
Malaria_Parasite_Dat <- read.csv(here("Data", "MALARIA_PAK_SPECIES.csv"))

###########
###Reptile#
###########

REP_Host_Dat <- subset(Malaria_Host_Dat,
                          Malaria_Host_Dat$Group == 'reptile')
REP_Phylo <- read.nexus(here('Data',"Reptile","reptile_phylo_1000.nex")) 
###1000 phylogenetic tree


###########
###Avian###
###########
AVE_Host_Dat <- subset(Malaria_Host_Dat,
                         Malaria_Host_Dat$Group == 'avian')
AVE_Phylo <- read.nexus(here("Data", "Avian", "avian_phylo_1000.nex"))
###1000 phylogenetic tree

###########
###Mammal##
###########
MAM_Host_Dat <- subset(Malaria_Host_Dat,
                         Malaria_Host_Dat$Group == 'mammal')
MAM_Phylo <- read.nexus(here("Data", "Mammal", "DNA_MAMMAL.tre"))

###For easier matching, I'm adding the underscore to my
###host data because the phylogenetic trees species' name have
###underscores

REP_Host_Dat$Species<- add_underscore(REP_Host_Dat$Species)
AVE_Host_Dat$Species <- add_underscore(AVE_Host_Dat$Species)
AVE_Host_Dat$Species[94] <- "Pulsatrix_koeniswaldiana"
MAM_Host_Dat$Species <- add_underscore(MAM_Host_Dat$Species)

###MAM_HOST_DAT (gets rid of the extraneous labels)- the capitalized
###parts of the string (upper taxon identifier) - one quirk
MAM_Phylo$tip.label<- stringr::str_extract(MAM_Phylo $tip.label, "[^_]*_[^_]*")

###Note that I already subsetted the information necessary for the 
###mammalian, avian , and reptilian

AVE_Merged <- Parasite_Data_Collector(AVE_Host_Dat,
                                       Malaria_Parasite_Dat)

MAM_Merged <- Parasite_Data_Collector(MAM_Host_Dat,
                                       Malaria_Parasite_Dat)

REP_Merged <- Parasite_Data_Collector(REP_Host_Dat,
                                      Malaria_Parasite_Dat)

#########################################################################
###As there can be multiple parasite species that infect a single host, #
###this function choose the greater upper burst size across all species #
###There is also a chance there is no upper limit, so I have decided to #
###omit the data                                                        #
#########################################################################

#warnings are about non-finite values but my functions 
#get rid of them of all (don't worry)

AVE_Merged_F <- Identifier_Burst_Size_Order(AVE_Merged)
MAM_Merged_F <- Identifier_Burst_Size_Order(MAM_Merged)
REP_Merged_F <- Identifier_Burst_Size_Order(REP_Merged)


###Because of the avian and reptilian phylogenetic trees are 1000- 
###I'm going to create a consensus tree

consensus_AVE_Tree <- Consensus(AVE_Phylo, p = 1, check.labels = TRUE)
AVE_Merged$Species[!(AVE_Merged$Species %in% consensus_AVE_Tree$tip.label)]
###Missing one but that is fine- it is only identified by genus 

consensus_REP_Tree <- Consensus(REP_Phylo, p = 1, check.labels = TRUE)
REP_Merged$Species[!(REP_Merged$Species %in% consensus_REP_Tree$tip.label)]
###Not missing any

###Most of our analysis is focused on upper burst size so let's do this
Avian_MERGED_F <- subset(AVE_Merged_F[[2]], 
                         AVE_Merged_F[[2]]$Species %in% 
                         consensus_AVE_Tree$tip.label)

Mammal_MERGED_F <- subset(MAM_Merged_F[[2]], 
                          MAM_Merged_F[[2]]$Species %in% 
                          MAM_Phylo$tip.label)

Reptile_MERGED_F <- subset(REP_Merged_F[[2]], 
                          REP_Merged_F[[2]]$Species 
                          %in% consensus_REP_Tree$tip.label)

###THIS PART IS FOR COLLECTING A SUPER TREE that
###has both reptile, mammal, and avian hosts.

tip.labels <- c("mam", "birds", "squam")

edge <- matrix(c(4,1,
                 4,5,
                 5,2,
                 5,3), 
               byrow=TRUE, ncol=2)

edge.length<- c(1,1,1,1)

Nnode <-2

ordertree <- list(edge=edge, Nnode=Nnode, tip.label=tip.labels )
class(ordertree) <- 'phylo'

###
tree_list <- list(squam=consensus_REP_Tree, 
                  birds=consensus_AVE_Tree, 
                  mam= MAM_Phylo)

class(tree_list) <- "multiPhylo"

Order_1_Reptile <- bind.tree(x=ordertree, y=consensus_REP_Tree, 
                   where = 3, interactive = FALSE)

Order_2_Reptile_Avian  <- bind.tree(x=Order_1_Reptile, y=consensus_AVE_Tree, 
                   where = 2, interactive = FALSE)

Order_3_Reptile_Avian_Mammal <- bind.tree(x=Order_2_Reptile_Avian , y = MAM_Phylo, 
                   where = 1, interactive = FALSE)

FULL_DAT <- rbind(Avian_MERGED_F,
                  Mammal_MERGED_F,
                  Reptile_MERGED_F)

FULL_ORDER_FINAL <- keep.tip(Order_3_Reptile_Avian_Mammal,FULL_DAT$Species)

FULL_Merged_Phylogeny<- phylo4d(
  FULL_ORDER_FINAL , 
  tip.data = FULL_DAT[,-2],
  match.data = TRUE)

Reptile_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(consensus_REP_Tree, 
           Reptile_MERGED_F$Species),
  tip.data =  Reptile_MERGED_F[,-2],
  match.data = TRUE
)

Mammal_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(MAM_Phylo, Mammal_MERGED_F$Species),
  tip.data =  Mammal_MERGED_F[,-2],
  match.data = TRUE
)

Avian_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(consensus_AVE_Tree, Avian_MERGED_F$Species),
  tip.data =  Avian_MERGED_F[,-2],
  match.data = TRUE
)
