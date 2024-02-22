### Cleaning up the phylogeny for the mammal, avian, and reptile
### hosts as well as including the pertinent data. This script
### is specifically for cleaning up the host phylogeny data
### without thinking about the parasites for modularity 

library(here)
source(here("Code","Functions","FUNC_Package_Loader.R"))
source(here("Code","Functions","FUNC_util_remove_underscore.R"))
source(here("Code","Functions","FUNC_Identifier_Burst_Size_Order.R"))

###REPTILE
REP_Phylo <- read.nexus(here('Data',"Reptile","reptile_phylo_1000.nex")) 
###1000 phylogenetic tree

###AVIAN
AVE_Phylo <- read.nexus(here("Data", "Avian", "avian_phylo_1000.nex"))
###1000 phylogenetic tree

###MAMMAL
MAM_Phylo <- read.nexus(here("Data", "Mammal", "mammal_phylo_1000.nex"))
###1000 phylogenetic trees

###Because the trees are all 1000- 
###I'm going to create a consensus tree (these have no branch length)
###p = 1 means that it's strict consensus

consensus_AVE_Tree <- consensus(AVE_Phylo, p = 0.5, check.labels = TRUE, rooted = TRUE)
consensus_REP_Tree <- consensus(REP_Phylo, p = 0.5, check.labels = TRUE,rooted = TRUE)
consensus_MAM_Tree <- consensus(MAM_Phylo, p = 0.5, check.labels = TRUE,rooted = TRUE)

consensus_AVE_Tree <- multi2di(consensus_AVE_Tree, random = TRUE)
consensus_REP_Tree <- multi2di(consensus_REP_Tree, random = TRUE)
consensus_MAM_Tree <- multi2di(consensus_MAM_Tree, random = TRUE)



###THIS PART IS FOR CREATING A SUPER TREE that
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
                  mam= consensus_MAM_Tree)

class(tree_list) <- "multiPhylo"




Order_1_Reptile <- bind.tree(x = ordertree, 
                             y = consensus_REP_Tree, 
                             where = 3, interactive = FALSE)

Order_2_Reptile_Avian  <- bind.tree(x = Order_1_Reptile, 
                                    y = consensus_AVE_Tree, 
                                    where = 2, 
                                    interactive = FALSE)

Full_SuperTree_Host <- bind.tree(x = Order_2_Reptile_Avian , 
                                 y = consensus_MAM_Tree, 
                                 where = 1, 
                                 interactive = FALSE)

