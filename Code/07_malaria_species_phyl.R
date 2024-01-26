library(here)
source(here("Code","Functions","Package_Loader.R"))
source(here("Code", "Functions","renamer_label.R"))
###Loading in the malaria parasite data (parasite_dat)
###as well as the phylogenetic
###tree (plasmodium_tree) 
parasite_dat <- read.csv(here("Data", "MALARIA_PAK_SPECIES.csv"))
plasmodium_tree <- read.nexus(here(file='Data','16_amino_acid_partitioned_BEAST_relaxed_clock.tre'))

###If you take a look, you can see that there are lot of non Plasmodium species-
###I looked at these by eye
###1-20, 37-38,54-59 for example print(plasmodium_tree$tip.label)

Non_Plas_species <- plasmodium_tree$tip.label[c(1:20,28:29,37:39,54:59)] ###These are non plasmodium species
Plas_species <-  plasmodium_tree$tip.label[-c(1:20,28:29,37:39,54:59)]

###This is the tree with only plasmodium species
plasmodium_tree_full <- drop.tip(plasmodium_tree , Non_Plas_species) 

###Giving the tree a new label- CHECK to make sure!
new_Plasmoidum_Species_Label <- c("azurophilum", "chiricahuae", "coatneyi","cyclopsi","floridense","fragile","gaboni",
   "lacertiliae","leucocytica", "mackerrasae","malariae", 'mexicanum','minuoviridae',
  'ovale', 'berghei', 'chabaudi','cynomolgi','falciparum','gallinaceum','giganteum',
  'inui', 'juxtanucleare','knowlesi','reichenowi', 'relictum','vinckei','vivax','yoelii')

plasmodium_tree_full <- rename_labels(tree = plasmodium_tree_full , vec = new_Plasmoidum_Species_Label)

###Check with the original plot 

###We get the parasite data (mine) that will be merged with the phylogenetic 
###tree:
###Subset Pak data so that only the species that appear in the phylogenetic
###tree is gotten.
parasite_data_sub <- subset(parasite_dat, parasite_dat$Plasmodium.species %in% 
                              new_Plasmoidum_Species_Label)
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