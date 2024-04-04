library(here)
source(here("Code","Functions", "FUNC_Package_Loader.R"))
source(here("Code", "Data_Analysis","00_parasite_phylogeny_tree.R"))

###Use the subsetted one
mal_dat_asex <- read.csv(here("Data","MALARIA_PHYLOGENY_HOST_VARIOUS.csv"))
mal_dat_asex$Host <- add_underscore(mal_dat_asex$Host)


###Pruning down the host trees for the analysis as well as making the cophylogenetic
###figure 

type_host_tree_subsetted <- drop.tip(Full_SuperTree_Host, 
                                     Full_SuperTree_Host$tip.label
                                     [-match(mal_dat_asex$Host, Full_SuperTree_Host$tip.label)])

                                     
vcv_Plasmodium <- vcv(compute.brlen(plasmodium_tree_full))
vcv_TypeHost <- vcv(compute.brlen(type_host_tree_subsetted))


###

model_null <- brm(
  Upper_Burst_Size ~ 1 +
    (1|gr(Plasmodium_species , cov = vcv_Plasmodium))+
    (1|gr(Host , cov = vcv_TypeHost)),
  data = mal_dat_asex ,
  family = poisson, warmup = 4000,
  iter = 10000,
  control = list(adapt_delta = 0.99999, 
                 max_treedepth = 20),
  data2 = list(vcv_Plasmodium = vcv_Plasmodium,
               vcv_TypeHost = vcv_TypeHost))


check_model(model_null)

performance::variance_decomposition(model_null, re_formula =  
                         ~(1|gr(Plasmodium_species , cov = vcv_Plasmodium)))

performance::variance_decomposition(model_null, re_formula =  
                                      ~(1|gr(Hosts , cov = vcv_TypeHost)))


