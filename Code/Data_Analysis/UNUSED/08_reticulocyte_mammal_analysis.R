###This is a code to look at the reticulocyte counts
###in the mammals to see if it has a relationship
###with the upper burst size 
library(here)
source(here("Code", "Functions", "Package_Loader.R"))
source(here("Code","05_phylogeny_full_combiner.R"))

###This is the mammal trait data that has all the information that
###I need
Mammal_trait_dat <- read.csv(here("Data","Mammal","Mammal_trait.csv"))

###Get tip label from the already finalized tree in the script that we sourced
Mammal_Tree_Species <- sub( "_"," ", row.names(tdata(Mammal_Data_Merged_Phylogeny ,"tip")))

###This subsets the trait data based on what was found in the phylogenetic
###tree
Mammal_Trait_Data <- subset(Mammal_trait_dat,    
                            Mammal_trait_dat$phylacine_binomial %in%
                            Mammal_Tree_Species )

###I see some duplicate entries (mistake(?) on the owner's)
Mammal_Trait_Data_2 <- Mammal_Trait_Data[!duplicated(Mammal_Trait_Data$phylacine_binomial), ]

###We want the adult_mass_g and the max_longevity_d
Mammal_Trait_Data_2_MASS <- Mammal_Trait_Data_2[,c("order",
                                                   "genus",
                                                   "phylacine_binomial",
                                                   "adult_mass_g", 
                                                   "max_longevity_d")]

###My hypothesis is that reticulocytes are 
### 

### Calculating the reticulocyte count from body mass
#We use "The Evolution of Mammalian Blood Parameters: 
#Patterns and Their Interpretation" by Promislow 1991
## log (reticulocytes) = 0.96 - 0.288 xlog(mass)$$


Mammal_Trait_Data_2_MASS$reticulocyte_log <-(((0.96 - 0.288 * log(Mammal_Trait_Data_2_MASS $adult_mass_g)))) 
Mammal_Trait_Data_2_MASS$phylacine_binomial <- sub(" ","_", Mammal_Trait_Data_2_MASS$phylacine_binomial)


ggplot(Mammal_Trait_Data_2_MASS, 
       aes(x = log(adult_mass_g), 
           y=(reticulocyte_log)))+
       geom_line()+
       geom_point(aes(fill= order), size = 4, shape = 21)+
       annotate("text", x= 10.5, y = -0.5,
                label = "log(reticulocytes) \n= 0.96 - 0.288 x log(mass)")+
       xlab("Adult mass (g) (log-transformed)")+
       ylab("Reticulocytes per 100 RBCs \n(log-transformed)")+
       scale_fill_viridis(discrete = TRUE, name = 'Mammal order')+
       theme_classic()+
       theme(axis.text = element_text(size = 14, color = 'black'),
             axis.title = element_text(size = 15, color = 'black'))
  
ggsave(here("Figures", "Raw", "mammal_retic_mass_compare.pdf"),
       height = 5.6, width = 7, units = 'in')

###combine the mammal trait data and the upper burst size

Data_Mammal_Mass_Retic <- merge(Mammal_MERGED_F,
                                    Mammal_Trait_Data_2_MASS, 
                                    by.x = "Species", 
                                    by.y = "phylacine_binomial")

row.names(Data_Mammal_Mass_Retic) <- Data_Mammal_Mass_Retic$Species

ggplot(Data_Mammal_Mass_Retic , 
       aes( x = reticulocyte_log, y = (Upper),
           color = genus))+
       geom_point(size =3) 


mammal_malaria <- comparative.data(phy = MAM_Phylo, 
                                   data = Data_Mammal_Mass_Retic, 
                                   names.col = Species, 
                                   vcv = TRUE, warn.dropped = TRUE)


model.pgls_Total<- pgls(Upper~reticulocyte_log~Upper,
                        data = mammal_malaria, lambda = "ML")
###Reticulocytes
model.pgls_Total<- pgls((Upper)~reticulocyte_log,
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

###Using compare.gee

MAM_Phylo_keep<- keep.tip(MAM_Phylo, Data_Mammal_Mass_Retic$Species)

a<- compar.gee(Upper~reticulocyte_log ,
           data = Data_Mammal_Mass_Retic, 
           family = "poisson", phy = MAM_Phylo_keep)
summary(a)
plot(a$residuals,a$fitted.values )
plot(Data_Mammal_Mass_Retic$reticulocyte_log,predict(a))

range_reticuloctes <-range(Data_Mammal_Mass_Retic$reticulocyte_log )

range_reticulocytes <- seq(range_reticuloctes[[1]], range_reticuloctes [[2]], length = 1000)
yweight <- predict(a, list(reticulocyte_log = range_reticulocytes),type="response")
plot(range_reticulocytes, yweight)

glm_retic_intercept <- (glm(Upper~1, data = Data_Mammal_Mass_Retic, family = 'poisson'))
glm_retic_1 <- glm(Upper~reticulocyte_log, data = Data_Mammal_Mass_Retic, family = 'poisson' )
glm_retic <- (glm(Upper~reticulocyte_log, data = Data_Mammal_Mass_Retic, family = 'poisson'))
summary(glm_retic_intercept)
summary(glm_retic_1)

summary(glm_retic )
plot(glm_retic)


plot((Upper)~reticulocyte_log, data =  Data_Mammal_Mass_Retic)


ggplot(Data_Mammal_Mass_Retic, aes(x = exp(reticulocyte_log), y = Upper,
                                   fill= family)) + 
  geom_point(size = 4, shape = 21) +
  scale_fill_viridis(discrete = "TRUE", option = 'turbo')+
  facet_wrap(~family)
  

