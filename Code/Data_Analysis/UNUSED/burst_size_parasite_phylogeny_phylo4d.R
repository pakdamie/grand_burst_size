
###We get the parasite data (mine) that will be merged with the phylogenetic 
###tree:
###Subset Pak data so that only the species that appear in the phylogenetic
###tree is gotten.


###Ensures that the duration is numeric
parasite_data_sub$Duration <- as.numeric(parasite_data_sub$Duration)

###I'm creating data.frames for the aexual duration and the upper burst size
###We have to make it separate because the package 
###doesn't like the NA entries in one but not the other!
parasite_data_sub_ASEXUAL <- na.omit(parasite_data_sub[,c(4,11)]) 
parasite_data_sub_DURATION <- na.omit(parasite_data_sub[,c(4,12)])
parasite_data_sub_FULL <- na.omit(parasite_data_sub[,c(4,11,12)])

###make sure the row names are the species name
row.names(parasite_data_sub_ASEXUAL) <- parasite_data_sub_ASEXUAL$Plasmodium.species
row.names(parasite_data_sub_DURATION) <- parasite_data_sub_DURATION$Plasmodium.species

###Cutting the tree a bit more based on if it either has the asexual
###duration or the upper burst size
plasmodium_tree_F2_ASEX <-keep.tip(plasmodium_tree_full,row.names(parasite_data_sub_ASEXUAL))
plasmodium_tree_F2_DURATION <-keep.tip(plasmodium_tree_full,row.names(parasite_data_sub_DURATION))

###Just ensuring that the order match just in case
parasite_sub_ASEXUAL <- parasite_data_sub_ASEXUAL[match(plasmodium_tree_F2_ASEX$tip.label,rownames(parasite_data_sub_ASEXUAL)),]
parasite_sub_DURATION <- parasite_data_sub_DURATION[match(plasmodium_tree_F2_DURATION$tip.label,rownames(parasite_data_sub_DURATION)),]

###
### When you source this code, you now have all the necessary data to 
### basically do the statistical tests.