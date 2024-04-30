source(here("Code", "Functions", "FUNC_Package_Loader.R"))
source(here("Code","Functions","FUNC_util_add_underscore.R"))
source(here("Code", "Data_Analysis", "00_full_HOST_phylogeny_tree.R"))
source(here("Code", "Data_Analysis","00_full_PARASITE_phylogeny_tree.R"))

mal_dat_asex <- read.csv(here("Data","MALARIA_PHYLOGENY_HOST_VARIOUS.csv"))
mal_dat_asex$Host <- add_underscore(mal_dat_asex$Host)


type_host_tree_subsetted <- drop.tip(Full_SuperTree_Host, 
                                     Full_SuperTree_Host$tip.label
                                     [-match(mal_dat_asex$Host, 
                                           Full_SuperTree_Host$tip.label)])

