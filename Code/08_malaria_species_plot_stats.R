library(here)
source(here("Code","Functions","Package_Loader.R"))
source(here("Code", "Functions","renamer_label.R"))
source(here("Code","07_malaria_species_phyl.R"))

### Match both the phylogeny and the data
PLAS_DATA_ASEXUAL <- phylo4d(plasmodium_tree_F2_ASEX, 
                             tip.data = parasite_sub_ASEXUAL[,-1] , 
                             match.data = TRUE)

PLAS_DATA_DURATION <- phylo4d(plasmodium_tree_F2_DURATION, 
                              tip.data = parasite_sub_DURATION[,-1] , 
                              match.data = TRUE)

MORAN_PLAS_DATA_ASEX <- abouheif.moran(PLAS_DATA_ASEXUAL,
                                       method = c("Abouheif"),
                                       nrepet = 5000)

MORAN_PLAS_DATA_DURATION <- abouheif.moran(PLAS_DATA_DURATION,
                                           method = c("Abouheif"),
                                           nrepet = 5000)

###
plot(MORAN_PLAS_DATA_ASEX, main = 'Upper burst size')
plot(MORAN_PLAS_DATA_DURATION, main = 'Asexual duration')


PLAS_DATA_ASEXUAL_GG <- gheatmap(ggtree(PLAS_DATA_ASEXUAL), 
                                data = tdata(PLAS_DATA_ASEXUAL),
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

###Ok what happens if we get rid of P. giganteum because 
### it has a super high burst size?

parasite_sub_ASEXUAL_removed_giganteum <- subset(parasite_sub_ASEXUAL,
                                                 parasite_sub_ASEXUAL$Plasmodium.species
                                                 != 'giganteum') 

tree_sub_ASEXUAL_removed_giganteum<- drop.tip(plasmodium_tree_F2_ASEX, "giganteum")

PLAS_DATA_ASEXUAL_removed_giganteum  <- phylo4d(tree_sub_ASEXUAL_removed_giganteum, 
                             tip.data =parasite_sub_ASEXUAL_removed_giganteum , 
                             match.data = TRUE)

MORAN_PLAS_DATA_ASEX_removed_giganteum <- 
                                       abouheif.moran(PLAS_DATA_ASEXUAL_removed_giganteum ,
                                       method = c("Abouheif"),
                                       nrepet = 5000)


###Ok what happens if we get rid of squamates?

parasite_sub_ASEXUAL_removed_reptile <- subset(parasite_sub_ASEXUAL,
                                                 !(parasite_sub_ASEXUAL$Plasmodium.species)
                                                 %in% c("azurophilum",
                                                        "chiricahuae",
                                                        "floridense",
                                                        "leucocytica",
                                                        "mackerrasae",
                                                        "mexicanum",
                                                        "giganteum"))

tree_sub_ASEXUAL_removed_reptile<- drop.tip(plasmodium_tree_F2_ASEX, c("azurophilum",
                                                                         "chiricahuae",
                                                                         "floridense",
                                                                         "leucocytica",
                                                                         "mackerrasae",
                                                                         "mexicanum",
                                                                         "giganteum"))

PLAS_DATA_ASEXUAL_removed_reptile  <- phylo4d(tree_sub_ASEXUAL_removed_reptile, 
                                                tip.data = parasite_sub_ASEXUAL_removed_reptile , 
                                                match.data = TRUE)

MORAN_PLAS_DATA_ASEX_removed_reptile <- 
  abouheif.moran(PLAS_DATA_ASEXUAL_removed_reptile,
                 method = c("Abouheif"),
                 nrepet = 5000,
                 alter = 'greater')

###This indicates to me that reptile malaria is the 

###this suggests that malaria species that rely on reptile hosts tend
###to have similar upper burst size but not for asexual duration