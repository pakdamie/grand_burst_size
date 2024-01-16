###This is the script where I run statistical tests about the 
###upper burst size and rather I can see a phylogenetic 
###signal from the host tree.

source(here("Code","05_phylogeny_full_combiner.R"))
###Ignore warning- it has to do with the non-finite values from the
###identifier_burst_size function (the function gets rid of them)

FULL_SUPER_TREE_ABOUHEIF.MORAN <- abouheif.moran(FULL_Merged_Phylogeny, nrepet = 1000)

plot(FULL_SUPER_TREE_ABOUHEIF.MORAN, main = "Upper burst")

REPTILE_TEST <- abouheif.moran(Reptile_Data_Merged_Phylogeny, nrepet = 1000)
MAMMAL_TEST <- abouheif.moran(Mammal_Data_Merged_Phylogeny, nrepet = 1000)
AVIAN_TEST <- abouheif.moran(Avian_Data_Merged_Phylogeny, nrepet = 1000)

plot(REPTILE_TEST,main = "Upper burst (Squamates)")
plot(MAMMAL_TEST,main = "Upper burst (Mammal")
plot(AVIAN_TEST, main = "Upper burst (Birds")

