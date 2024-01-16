
library(here)
source(here("Code", "Functions", "Package_Loader.R"))
source(here("Code","05_phylogeny_full_combiner.R"))

Mammal_trait_dat <- read.csv(here("Data","Mammal","Mammal_trait.csv"))

###Get tip label from the already finalized tree
Mammal_Tree_Species <- sub( "_"," ", row.names(tdata(Mammal_Data_Merged_Phylogeny ,"tip")))

###This subsets the trait data based on what was found in the phylogenetic
###Tree
Mammal_Trait_Data <- subset(Mammal_trait_dat,    
                            Mammal_trait_dat$phylacine_binomial %in%
                              Mammal_Tree_Species )

###I see some duplicate entries (mistake(?) on the owner's)
Mammal_Trait_Data_2 <- Mammal_Trait_Data[!duplicated(Mammal_Trait_Data$phylacine_binomial), ]

###We want the adult_mass_g and the max_longevity_d
Mammal_Trait_Data_2_MASS <- Mammal_Trait_Data_2[,c("order","family",
                                                   "genus",
                                                   "phylacine_binomial",
                                                   "adult_mass_g", 
                                                   "max_longevity_d")]

### Calculating the reticulocyte count from body mass

#We use "The Evolution of Mammalian Blood Parameters: 
#Patterns and Their Interpretation" by Promislow 1991
## log (reticulocytes) = 0.96 - 0.288 xlog(mass)$$


Mammal_Trait_Data_2_MASS$reticulocyte_log <-(((0.96 - 0.288 * log(Mammal_Trait_Data_2_MASS $adult_mass_g)))) 
Mammal_Trait_Data_2_MASS$reticulocyte_log_transformed <- sign(Mammal_Trait_Data_2_MASS$reticulocyte_log)*abs(Mammal_Trait_Data_2_MASS$reticulocyte_log)^1/3
Mammal_Trait_Data_2_MASS$phylacine_binomial <- sub(" ","_", Mammal_Trait_Data_2_MASS$phylacine_binomial)


ggplot(Mammal_Trait_Data_2_MASS, 
       aes(x = (adult_mass_g), y=reticulocyte_log))+
       geom_point(size=2)+
       xlab("Adult mass (g)")+
       ylab("Reticulocytes (log-transformed)")+theme_classic()
  


###combine the mammal trait data and the upper burst size

Data_Mammal_Mass_Retic <- merge(Mammal_MERGED_F,
                                    Mammal_Trait_Data_2_MASS, 
                                    by.x = "Species", 
                                    by.y = "phylacine_binomial")

row.names(Data_Mammal_Mass_Retic) <- Data_Mammal_Mass_Retic$Species


mammal_malaria <- comparative.data(phy = MAM_Phylo, 
                                   data = Data_Mammal_Mass_Retic, 
                                   names.col = Species, 
                                   vcv = TRUE, warn.dropped = TRUE)

###Reticulocytes
model.pgls_Total<- pgls(sqrt(Upper)~reticulocyte_log,
                        data = mammal_malaria, lambda = "ML")
summary(model.pgls_Total)

par(mfrow=c(2,2))
plot(model.pgls_Total)

model.pgls_Subset_1<- pgls(log(upper)~ log(max_longevity_d) , 
                           data = mammal_malaria, lambda = "ML")
summary(model.pgls_Subset_1)

model.pgls_Subset_2<- pgls(log(Upper)~ reticulocyte_log +log(max_longevity_d), 
                           data = mammal_malaria, lambda = "ML")
summary(model.pgls_Subset_2)

AIC(model.pgls_Total, model.pgls_Subset_1,model.pgls_Subset_2)