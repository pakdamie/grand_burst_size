###Figure making script that looks at the 
###asexual development time and the maximum burst size to see if
###there is any relationship between them.
###This is the plot making figure

library(here)
source(here("Code","Functions", "FUNC_Package_Loader.R"))
source(here("Code", "Data_Analysis","00_parasite_phylogeny_tree.R"))

###Malaria Data - Species
mal_dat_asex <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

###Ensures that we're not looking at malaria species that were excluded
mal_dat_asex <- subset(mal_dat_asex, mal_dat_asex$Include != "No")

###Subset the data
subsetted_mal_dat <- mal_dat_asex [,c("Plasmodium.species",
                                        "Average",
                                        "Lower",
                                        "Upper",
                                        "Duration",
                                        "RBC_preference",
                                        "Type.Host",
                                        "OrderHost")]

###Make sure I'm not getting any data that has missing duration data
subsetted_mal_dat <- subsetted_mal_dat[is.na(subsetted_mal_dat $Duration) == FALSE 
                                       & subsetted_mal_dat $Duration != "",]
subsetted_mal_dat$Duration <- as.numeric(subsetted_mal_dat$Duration)
subsetted_mal_dat$Upper <- as.numeric(subsetted_mal_dat$Upper)

###Plasmodium species ###
###This is to look for the malaria species that are both in the data
###and the tree data.
Plasmodium_Mal_Subset <- intersect(subsetted_mal_dat$Plasmodium.species, 
                                   plasmodium_tree_full$tip.label)

###The Plasmodium tree
plasmodium_tree_subsetted <- keep.tip(plasmodium_tree_full,
                                      Plasmodium_Mal_Subset)

###The Plasmodium data
Plasmodium_Found_Phylogeny <- subset(subsetted_mal_dat, 
                                      subsetted_mal_dat$Plasmodium.species %in%
                                      Plasmodium_Mal_Subset)

###Now looking for the corresponding type host tree data
Host_Found <- sub(" ", "_", Plasmodium_Found_Phylogeny$Type.Host)
Plasmodium_Found_Phylogeny$Type.Host <- sub(" ", "_", Plasmodium_Found_Phylogeny$Type.Host)

###The host tree
Host_tree_subsetted <- keep.tip(Full_SuperTree_Host,Host_Found )

vcv_Plasmodium <- vcv(compute.brlen(plasmodium_tree_subsetted))
vcv_Host <- vcv(compute.brlen(Host_tree_subsetted))

###Check to see if there 

model_Duration_Null_Both <- brm(
 Upper ~  Duration + RBC_preference+
   (1|gr(Plasmodium.species , cov = vcv_Plasmodium))+
   (1|gr(Type.Host , cov = vcv_TypeHost)),
 data = Plasmodium_Found_Phylogeny  ,
  family =poisson(),
  warmup = 4000,
  iter = 15000,
  control = list(adapt_delta = 0.99999, max_treedepth = 20),
 data2 = list(vcv_Plasmodium = vcv_Plasmodium,
              vcv_TypeHost = vcv_TypeHost ))


model_Duration_Null_RBC_Preference <- brm(
  Upper ~ RBC_preference+
    (1|gr(Plasmodium.species , cov = vcv_Plasmodium))+
    (1|gr(Type.Host , cov = vcv_TypeHost)),
  data = Plasmodium_Found_Phylogeny  ,
  family =poisson(),
  warmup = 4000,
  iter = 15000,
  control = list(adapt_delta = 0.99999, max_treedepth = 20),
  data2 = list(vcv_Plasmodium = vcv_Plasmodium,
               vcv_TypeHost = vcv_TypeHost ))
plot(marginal_effects(model_Duration_Null_RBC_Preference), ask = FALSE)



UPPER_GG  <- ggplot(subsetted_mal_dat, 
                    aes(x = (Duration), 
                        y = Upper, 
                        fill = as.factor(Duration),
                        color = as.factor(Order.Host)))+
  geom_beeswarm(size = 4, shape = 21)+
  scale_fill_viridis(option = 'turbo', discrete = TRUE)+
  scale_x_continuous(breaks = c(0,24,48, 72, 96))+
  scale_y_continuous(limits = c(0,40), 
                     breaks = seq(0,40,5))+
  xlab("Duration (Hours)") +
  ylab("Upper burst size") +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))



Age_Preference <- ggplot(subset(mal_dat_asex, 
                                is.na(mal_dat_asex$RBC_preference) ==FALSE),
                    aes(x = RBC_preference, 
                        y = Upper))+
  geom_beeswarm(size = 4, shape = 21)+
  scale_fill_viridis(option = 'turbo', discrete = TRUE)+
  xlab("Duration (Hours)") +
  ylab("Upper burst size") +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))

Age_Preference



ggsave(here("Figure","Data_Analysis","Burst_Size_AesxualDuration_Figure.pdf"),
       width = 10, height = 4, units = 'in')


###SUPPLEMENT

LOWER_GG  <- ggplot(subsetted_mal_dat, 
                    aes(x = (Duration), 
                        y = Lower, 
                        fill = as.factor(Duration)))+
  geom_beeswarm(size = 3, shape = 21)+
  scale_fill_viridis(option = 'viridis', discrete = TRUE)+
  scale_x_continuous(breaks = c(0,24,48, 72, 96))+
  scale_y_continuous(limits = c(0,30), 
                     breaks = seq(0,30,5))+
  xlab("Duration (Hours)") +
  ylab("Lower burst size") +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))

ggsave(here("Figure","Data_Analysis","SUPP_LOWER_Burst_Size_AesxualDuration_Figure.pdf"),
       width = 10, height = 4, units = 'in')




Average_GG  <- ggplot(Full_Mal_Duration_Data, 
                      aes(x = (Duration), 
                          y = Average, 
                          fill = as.factor(Duration)))+
  geom_beeswarm(size = 3, shape = 21)+
  scale_fill_viridis(option = 'viridis', discrete = TRUE)+
  scale_x_continuous(breaks = c(0,24,48, 72, 96))+
  scale_y_continuous(limits = c(0,30), 
                     breaks = seq(0,30,5))+
  xlab("Duration (Hours)") +
  ylab("Average burst size") +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))

ggsave(here("Figure","Data_Analysis","SUPP_AVG_Burst_Size_AesxualDuration_Figure.pdf"),
       width = 10, height = 4, units = 'in')
