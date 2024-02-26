library(here)
library(varde)
source(here("Code","Functions", "FUNC_Package_Loader.R"))
source(here("Code", "Data_Analysis","00_parasite_phylogeny_tree.R"))

###Malaria Data - Species
mal_dat_asex <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))


###Ensures that we're not lookingg at malaria species 
mal_dat_asex <- subset(mal_dat_asex, mal_dat_asex$Include != "No")

###Subset the data
subsetted_mal_dat <- mal_dat_asex [,c("Plasmodium.species",
                                      "Average",
                                      "Lower",
                                      "Upper",
                                      "Type.Host",
                                      "OrderHost")]

Plasmodium_Mal_Subset <- intersect(subsetted_mal_dat$Plasmodium.species, 
                                   plasmodium_tree_full$tip.label)
###The Plasmodium tree
plasmodium_tree_subsetted <- keep.tip(plasmodium_tree_full,
                                      Plasmodium_Mal_Subset)

plasmodium_data_subsetted <- subset(subsetted_mal_dat, 
                                     subsetted_mal_dat$Plasmodium.species %in%
                                       Plasmodium_Mal_Subset)
plasmodium_data_subsetted $Type.Host <- add_underscore(
  Plasmodium_data_subsetted $Type.Host)
###The type host tree 
type_host_tree_subsetted <- keep.tip(Full_SuperTree_Host, 
              plasmodium_data_subsetted $Type.Host                      
)

vcv_Plasmodium <- vcv(compute.brlen(plasmodium_tree_subsetted))
vcv_TypeHost <- vcv(compute.brlen(type_host_tree_subsetted))


###

plasmodium_data_subsetted$parasite_name <- plasmodium_data_subsetted$Plasmodium.species
plasmodium_data_subsetted$host_name <- plasmodium_data_subsetted$Type.Host
plasmodium_data_subsetted$observation <- seq(1, nrow(plasmodium_data_subsetted))

model_null <- brm(
  Upper ~ 1 +
    (1|gr(Plasmodium.species , cov = vcv_Plasmodium))+
    (1|gr(Type.Host , cov = vcv_TypeHost)),
  data = plasmodium_data_subsetted ,
  family = poisson,warmup = 4000,
  iter = 10000,
  control = list(adapt_delta = 0.99999, max_treedepth = 20),
  data2 = list(vcv_Plasmodium = vcv_Plasmodium,
               vcv_TypeHost = vcv_TypeHost ))

###All random effects (total variance) 
PPD <- brms::posterior_predict(model_null, re_formula = NA) #re_formual = ALL random effect
var_total <- apply(PPD, MARGIN = 1, FUN = stats::var) #the total variance 

###Fixed effects (fixed variance)
PPD_0 <- brms::posterior_predict(model_null, re_formula = NULL) #re_formual = ALL random effect
var_fixed <- apply(PPD_0, MARGIN = 1, FUN = stats::var)

###Host (random variance)
PPD_H<- brms::posterior_predict(model_null, re_formula =  
                                  ~(1|gr(Type.Host , cov = vcv_TypeHost))) #re_formual = ALL random effect
var_host <- apply(PPD_H, MARGIN = 1, FUN = stats::var)

###Host (random variance)
PPD_P<- brms::posterior_predict(model_null, re_formula =  
                                  ~(1|gr(Plasmodium.species , cov = vcv_Plasmodium))) #re_formual = ALL random effect
var_parasite<- apply(PPD_P, MARGIN = 1, FUN = stats::var)

(var_parasite ) /(var_total)

performance::variance_decomposition(model_null, re_formula =  
                         ~(1|gr(Plasmodium.species , cov = vcv_Plasmodium)))

performance::variance_decomposition(model_null, re_formula =  
                                      ~(1|gr(Type.Host , cov = vcv_TypeHost)))


