###This script is to plot and explore if there is any 
###relationship with the upper burst size and
###RBC_preference

library(here)
source(here("Code","Functions", "Package_Loader.R"))

###Malaria Data - Species
malaria_species_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

malaria_species_dat <- subset(malaria_species_dat ,malaria_species_dat $Include != "No")
###Subset the data
subsetted_mal_dat <-   malaria_species_dat[,c("Plasmodium.species",
                                  "Average",
                                  "Lower",
                                  "Upper",
                                  "RBC_preference",    
                                  "Duration",
                                  "OrderHost")]

###I am only interested in the small subset of the Plasmodium 
###parasite species data that have any information on the RBC_preference!
subsetted_mal_dat_preference <- subset(subsetted_mal_dat,
                                      subsetted_mal_dat$RBC_preference != " " &
                                      subsetted_mal_dat$RBC_preference != "" & 
                                        subsetted_mal_dat$Plasmodium.species != "circumflexum_1")
                                


subsetted_mal_dat_preference$RBC_preference <- 
  factor(subsetted_mal_dat_preference$RBC_preference,
         levels = c("All","Retics","Mature"))

RBC_Preference_GG <- ggplot(subsetted_mal_dat_preference , 
       aes(x = RBC_preference, y = Upper, fill =RBC_preference)) + 
       geom_boxplot(alpha = 0.4) +
       geom_beeswarm(shape = 21, dodge.width=.8,cex=2, size = 3) +
       scale_fill_viridis(option = 'mako', discrete = TRUE)+
       xlab("RBC preference") +
       ylab("Upper burst size") +
       theme_classic() +
       theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))

RBC_Preference_GG 


###Subset the data
subsetted_mal_dat_2 <-   na.omit(malaria_species_dat[,c("Plasmodium.species",
                                              "Immature_Burst",
                                              "Immature_Mature")])


colnames(subsetted_mal_dat_2) <- c("Species", "Reticulocyte", "Mature")

subsetted_mal_dat_2_melted <- melt(subsetted_mal_dat_2, id.vars = "Species")

Squamatian_Preference <- ggplot(subsetted_mal_dat_2_melted ,aes (x = variable, y = value, 
                                        color = Species,
                                 group = Species))+
  geom_line() +geom_point(size = 3)+
  xlab("RBC preference") +
  ylab("Burst size") +
  ggtitle("Squamatian Plasmoidum")+
  theme_classic() + 
  scale_color_viridis(discrete = TRUE, option = 'turbo') + 
  theme(axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15))


 ggsave(here("Figures","Raw",
            "Lower_Burst_Size_Preference_Figure.pdf"),
       width = 8, height = 5, units = 'in')


############################
###Burst size in chabaudi###

burst_size_Chabaudi <- read.csv(here("Data","Chabaudi_Burst_Sizediff.csv"))

burst_size_Chabaudi$id <- seq(1, nrow(burst_size_Chabaudi))

burst_size_Chabaudi_melt <- melt(burst_size_Chabaudi, 
                                 id.vars=c("Species","Genotype", "Type", 'id'))

burst_size_Chabaudi_melt $variable <- factor(burst_size_Chabaudi_melt $variable ,
                                             levels=c('All',"Retic","Mature")) 

Chabaudi_Melt_GG <- ggplot(na.omit(burst_size_Chabaudi_melt), 
       aes(x = variable, y = value, group = id, color = Type))+
      geom_point(size = 3)+geom_line()+
      xlab("RBC preference")+
      ylab("Average burst size") + 
      scale_color_manual(values = c("#551fbd","#aadac6"))+
      facet_wrap(~Type, scales = 'free_y', ncol = 2)+
      theme_classic() + 
      theme(axis.text = element_text(size = 14, color = 'black'),
            axis.title = element_text(size = 15, color = 'black'),
            strip.background  = element_blank(),
            strip.text = element_text(size = 15))
Chabaudi_Melt_GG



Squamatian_Preference  /Chabaudi_Melt_GG

###
malaria_agepref_dat <- read.csv(here("Data","Age_Preference_2.csv"))

ggplot(malaria_agepref_dat, aes( x= Preference, y= Species)) + geom_point()+
  theme_classic()


ggsave(here("Figures","Raw",
            "RBC_age_preference_data.pdf"),
       width = 12, height = 6, units = 'in')
