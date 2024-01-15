
FULL_TEST <- abouheif.moran(FULL_Merged_Phylogeny, nrepet = 5000)

REPTILE_TEST <- abouheif.moran(Reptile_Data_Merged_Phylogeny, nrepet = 1000)
MAMMAL_TEST <- abouheif.moran(Mammal_Data_Merged_Phylogeny, nrepet = 1000)
AVIAN_TEST <- abouheif.moran(Avian_Data_Merged_Phylogeny, nrepet = 1000)

plot(FULL_TEST)
