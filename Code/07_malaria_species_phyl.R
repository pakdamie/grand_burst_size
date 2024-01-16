library(ape)
library(here)
library(caper)
library(phylobase)
library(nlme)
library(adephylo)
######################################
###Inputting the malaria species data#
######################################
parasite_dat <- read.csv(here("Data", "MALARIA_PAK_SPECIES.csv"))
plasmodium_tree <- read.nexus(here(file='Data','16_amino_acid_partitioned_BEAST_relaxed_clock.tre'))

rename_labels <- function(tree, vec) {
  tree$tip.label <- vec
  return(tree)
}
#####################################
###Plasmodium tree - not subsetted###
#####################################

###1-20, 37-38,54-59
Non_Plas_species <- plasmodium_tree $tip.label[c(1:20,28:29,37:39,54:59)] ###These are non plasmodium species
Plas_species <-  plasmodium_tree $tip.label[-c(1:20,28:29,37:39,54:59)]

plasmodium_tree_full <- drop.tip(plasmodium_tree , Non_Plas_species) ###This is the tree with only plas. species

###Giving the tree a new label- CHECK to make sure!
new_Plasmoidum_Species_Label <- c("azurophilum", "chiricahuae", "coatneyi","cyclopsi","floridense","fragile","gaboni",
   "lacertiliae","leucocytica", "mackerrasae","malariae", 'mexicanum','minuoviridae',
  'ovale', 'berghei', 'chabaudi','cynomolgi','falciparum','gallinaceum','giganteum',
  'inui', 'juxtanucleare','knowlesi','reichenowi', 'relictum','vinckei','vivax','yoelii')

plasmodium_tree_full <- rename_labels(tree = plasmodium_tree_full , vec = new_Plasmoidum_Species_Label)


###We get the parasite data (mine) that will be merged with the phylogenetic 
###tree
parasite_data_sub <- subset(parasite_dat, parasite_dat$Plasmodium.species %in% new_Plasmoidum_Species_Label)
parasite_data_sub$Duration <- as.numeric(parasite_data_sub$Duration)

###We have to make it seperate because the package 
###doesn't like the NA entries in one but not the other
parasite_data_sub_ASEXUAL <- na.omit(parasite_data_sub[,c(4,11)]) 
parasite_data_sub_DURATION <- na.omit(parasite_data_sub[,c(4,12)])
parasite_data_sub_FULL <- na.omit(parasite_data_sub[,c(4,11,12)])

parasite_data_sub_DURATION $Duration <- as.numeric(parasite_data_sub_DURATION $Duration)
parasite_data_sub_FULL$Duration <- as.numeric(parasite_data_sub_FULL$Duration)

###make sure the row names are the species name
row.names(parasite_data_sub_ASEXUAL) <- parasite_data_sub_ASEXUAL$Plasmodium.species
row.names(parasite_data_sub_DURATION) <- parasite_data_sub_DURATION$Plasmodium.species

plasmodium_tree_F2_ASEX <-keep.tip(plasmodium_tree_full,row.names(parasite_data_sub_ASEXUAL))
plasmodium_tree_F2_DURATION <-keep.tip(plasmodium_tree_full,row.names(parasite_data_sub_DURATION))


parasite_sub_ASEXUAL <- parasite_data_sub_ASEXUAL[match(plasmodium_tree_F2_ASEX$tip.label,rownames(parasite_data_sub_ASEXUAL)),]
parasite_sub_DURATION <- parasite_data_sub_DURATION[match(plasmodium_tree_F2_DURATION$tip.label,rownames(parasite_data_sub_DURATION)),]

###
PLAS_DATA_ASEXUAL <- phylo4d(plasmodium_tree_F2_ASEX, 
                             tip.data = parasite_sub_ASEXUAL , 
                              match.data = TRUE)
PLAS_DATA_DURATION <- phylo4d(plasmodium_tree_F2_DURATION, tip.data = parasite_sub_DURATION[,-1] , 
                             match.data = TRUE)

MORAN_PLAS_DATA_ASEX <- abouheif.moran(PLAS_DATA_ASEXUAL,method = c("Abouheif"),
                                       nrepet = 1000)
MORAN_PLAS_DATA_DURATION <- abouheif.moran(PLAS_DATA_DURATION,
                                           method = c("Abouheif"),
                                           nrepet = 1000)

###
plot(MORAN_PLAS_DATA_ASEX, main = 'Upper burst size')
plot(MORAN_PLAS_DATA_ASEX, main = 'Asexual duration')


PLAS_DATA_ASEXUAL_GG<- gheatmap(ggtree(PLAS_DATA_ASEXUAL), data =tdata(PLAS_DATA_ASEXUAL),
                     low = "#0a50a1",
                     high = "#e1000c",
                     color = "black",
                     width = 0.10,
                     legend_title  = "Upper \n burst size") +  
                     geom_tiplab(offset = 0.1)+
                     hexpand(.2) + vexpand(.05, -1) + 
                     theme(legend.position = c(.1, .75))


ggsave(here("Figure","Raw","Supp_Burst_Asexual_Phylogeny.pdf"), width = 8,
       height = 10, units = 'in')


###GLS