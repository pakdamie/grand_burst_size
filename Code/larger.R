
###As there can be multiple parasite species that infect a single host, 
###this function choose the greater upper burst size across all species 
###There is also a chance there is no upper limit, so I have decided to 
###omit the data                                                        

#warnings are about non-finite values but my functions 
#get rid of them of all (don't worry)

AVE_Merged_F <- Identifier_Burst_Size_Order(AVE_Merged)
MAM_Merged_F <- Identifier_Burst_Size_Order(MAM_Merged)
REP_Merged_F <- Identifier_Burst_Size_Order(REP_Merged)


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


FULL_DAT <- data.frame(rbind(Avian_MERGED_F,
                             Mammal_MERGED_F,
                             Reptile_MERGED_F))[,-2, drop = FALSE]


FULL_ORDER_FINAL <-keep.tip(Order_3_Reptile_Avian_Mammal ,row.names(FULL_DAT))

FULL_Merged_Phylogeny<- phylo4d(
  FULL_ORDER_FINAL ,
  tip.data = FULL_DAT,
  match.data = TRUE)

Reptile_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(consensus_REP_Tree, 
           Reptile_MERGED_F$Species),
  tip.data =  FULL_DAT,
  match.data = TRUE
)

Mammal_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(MAM_Phylo, Mammal_MERGED_F$Species),
  tip.data =  FULL_DAT,
  match.data = TRUE
)

Avian_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(consensus_AVE_Tree, Avian_MERGED_F$Species),
  tip.data =  FULL_DAT,
  match.data = TRUE
)

