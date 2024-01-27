
tip.labels <- c("mam", "birds", "squam")

edge <- matrix(c(4,1,
                 4,5,
                 5,2,
                 5,3), 
               byrow=TRUE, ncol=2)

edge.length<- c(1,1,1,1)

Nnode <-2

ordertree_mbs<- list(edge=edge, Nnode=Nnode, tip.label=tip.labels )
class(ordertree_mbs) <- 'phylo'

###
tree_list_mbs <- list(squam=NA, 
                  birds=consensus_AVE_Tree, 
                  mam= MAM_Phylo)

class(tree_list_mbs ) <- "multiPhylo"

Order_1_Avian  <- bind.tree(x=ordertree_mbs, y=consensus_AVE_Tree, 
                                    where = 2, interactive = FALSE)

Order_2_Avian_Mammal <- bind.tree(x= Order_1_Avian , 
                                  y = MAM_Phylo, 
                                      where = 1, interactive = FALSE)


FULL_DAT_am <- data.frame(rbind(Avian_MERGED_F,
                             Mammal_MERGED_F))[,-2, drop = FALSE]


FULL_ORDER_FINAL_am <-keep.tip(Order_2_Avian_Mammal  ,row.names(FULL_DAT_am))

FULL_Merged_Phylogeny_am<- phylo4d(
  FULL_ORDER_FINAL_am ,
  tip.data = FULL_DAT_am,
  match.data = TRUE)


AM_SUPER_TREE_ABOUHEIF.MORAN <- abouheif.moran(FULL_Merged_Phylogeny_am, nrepet =5000)

###Still super significant

