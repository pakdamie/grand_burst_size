###This is to do analysis on the Plasmodium 
###phylogeny itself
library(here)
source(here("Code","Functions","FUNC_Package_Loader.R"))
source(here("Code", "Functions","FUNC_renamer_label.R"))

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
plasmodium_tree_full<- drop.tip(plasmodium_tree_full,c("minuoviridae",
                                                       "lacertiliae",
                                                       "gaboni",
                                                       "leucocytica"))
