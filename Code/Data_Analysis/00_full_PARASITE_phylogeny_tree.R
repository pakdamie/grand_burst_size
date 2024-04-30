###@ 00_full_PARASITE_phylogeny_tree.R

### This is the full phylogeny for the Plasmodium species 
### This script is specifically for cleaning up the Plasmoidum phylogeny 
### data.

source(here("Code", "Functions","FUNC_renamer_label.R"))

###Loading in the malaria parasite data (parasite_dat)
###as well as the phylogenetic tree (plasmodium_tree) 
plasmodium_tree <- read.nexus(here(file='Data','16_amino_acid_partitioned_BEAST_relaxed_clock.tre'))

### Deer_Malaria
###Some string start with P_
matching_P_string <- plasmodium_tree$tip.label[grep("^P_", plasmodium_tree$tip.label)]
###Some string start with 
matching_Plas_string<- plasmodium_tree$tip.label[grep("^Plas_", plasmodium_tree$tip.label)]

### Come vector of Plasmoidum species name
Plasmodium_species_names <- c("Deer_Malaria",matching_P_string, matching_Plas_string)

###This is the tree with only plasmodium species
plasmodium_tree_full <- keep.tip(plasmodium_tree , Plasmodium_species_names ) 

###Giving the tree a new label- CHECK to make sure!
new_Plasmoidum_Species_Label <- c("odocoilei",
                                  "azurophilum", 
                                  "chiricahuae", 
                                  "coatneyi",
                                  "cyclopsi",
                                  "floridense",
                                  "fragile",
                                  "gaboni",
                                  "gemini",
                                  "koreafense",
                                  "lacertiliae",
                                  "leucocytica", 
                                  "mackerrasae",
                                  "malariae", 
                                  "mexicanum",
                                  "minuoviridae",
                                  "ovale", 
                                  "turdus",
                                  "berghei", 
                                  "chabaudi",
                                  "cynomolgi",
                                  "falciparum",
                                  "gallinaceum",
                                  "giganteum",
                                  "inui", 
                                  "juxtanucleare",
                                  "knowlesi",
                                  "reichenowi",
                                  "relictum",
                                  "vinckei",
                                  "vivax",
                                  "yoelii")

plasmodium_tree_full <- rename_labels(tree = plasmodium_tree_full , vec = new_Plasmoidum_Species_Label)

###The plasmodium_tree_full is the full one
plasmodium_tree_full <- drop.tip(plasmodium_tree_full,c("gemini",
                                                       "minuoviridae",
                                                       "koreafense",
                                                       "leucocytica",
                                                       "gaboni",
                                                       "turdus"))
