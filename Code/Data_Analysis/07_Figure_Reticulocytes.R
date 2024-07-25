###This is a code to look at the reticulocyte counts
###in the mammals to see if it has a relationship
###with the upper burst size 
library(here)
source(here("Code", "Functions", "FUNC_DA_full_data_loader.R"))
Mammal_trait_dat <- read.csv(here("Data","Mammal","Mammal_trait.csv"))

###Get tip label from the already finalized tree in the script that we sourced
Mammal_Tree_Species <- consensus_MAM_Tree$tip.label

Mammal_trait_dat$phylacine_binomial <- add_underscore(Mammal_trait_dat$phylacine_binomial )
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
                                                   "adult_mass_g"
                                                   )]

### Calculating the reticulocyte count from body mass
#We use "The Evolution of Mammalian Blood Parameters: 
#Patterns and Their Interpretation" by Promislow 1991
## log (reticulocytes) = 0.96 - 0.288 xlog(mass)$$


Mammal_Trait_Data_2_MASS$reticulocyte_log <-(((0.96 - 0.288 * log(Mammal_Trait_Data_2_MASS $adult_mass_g)))) 
Mammal_Trait_Data_2_MASS$phylacine_binomial <- sub(" ","_", Mammal_Trait_Data_2_MASS$phylacine_binomial)


#ggplot(Mammal_Trait_Data_2_MASS, 
#       aes(x = log(adult_mass_g), 
#           y=(reticulocyte_log)))+
#  geom_line()+
#  geom_point(aes(fill= order), size = 4, shape = 21)+
#  annotate("text", x= 10.5, y = -0.5,
#           label = "log(reticulocytes) \n= 0.96 - 0.288 x log(mass)")+
#  xlab("Adult mass (g) (log-transformed)")+
#  ylab("Reticulocytes per 100 RBCs \n(log-transformed)")+
#  scale_fill_viridis(discrete = TRUE, name = 'Mammal order')+
#  theme_classic()+
#  theme(axis.text = element_text(size = 14, color = 'black'),
#        axis.title = element_text(size = 15, color = 'black'))

#ggsave(here("Figures", "Raw", "mammal_retic_mass_compare.pdf"),
#       height = 5.6, width = 7, units = 'in')

Data_Mammal_Mass_Retic <- left_join(Mammal_Trait_Data_2_MASS,
           mal_dat_asex_full[,c("Plasmodium.species","Type_Host","Upper","Known_Preference")],
           by= c('phylacine_binomial'= "Type_Host"), multiple = 'all')

Data_Mammal_Mass_Retic <- Data_Mammal_Mass_Retic[is.na(Data_Mammal_Mass_Retic$Plasmodium.species) ==FALSE,]


Data_Mammal_Mass_Retic$Known_Preference2 <- ifelse(Data_Mammal_Mass_Retic$Known_Preference == "Immature", "Yes","No")

Mammal_Mass_Retic_GG<- ggplot(Data_Mammal_Mass_Retic, 
       aes(x = exp(reticulocyte_log), 
           y=(Upper)))+
         geom_point(aes(color = Known_Preference2),
                    size = 3)+
        scale_color_manual(values = c("No" = "darkgrey",
                                     "Yes" = "#fe64a3"),
                                     na.value="darkgrey")+
        xlab("Reticulocyte proportion")+
        ylab("Maximum observed burst size")+
        theme_classic()+
        theme(legend.position = 'none',
                axis.text = element_text(size = 14, color = 'black'),
              axis.title = element_text(size = 15, color = 'black'))
  

###### ECO + EVO
#model_ecophylo <- brm(
#  Upper ~ log(max_longevity_d),
#  data = Data_Mammal_Mass_Retic,
#  family = poisson("log"),
#  warmup = 2000,
#  iter = 6000,
#  control = list(
#    adapt_delta = 0.999,
#    max_treedepth = 11
#  ),
#  data2 = list(
#    vcv_Plasmodium_standard = vcv_Plasmodium_standard,
#    vcv_Host_standard = vcv_Host_standard
#  ),
#  save_pars = save_pars(all = TRUE)
#)




