###This script is to plot and explore if there is any 
###relationship with the upper burst size and
###RBC_preference

library(here)
source(here("Code","Functions", "FUNC_Package_Loader.R"))

###Malaria Data - Species
malaria_species_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))
malaria_species_dat <- subset(malaria_species_dat ,malaria_species_dat $Include != "No")
malaria_species_dat2 <- read.csv(here("Data", "MALARIA_PHYLOGENY_HOST_VARIOUS.csv"))
burst_size_Chabaudi <- read.csv(here("Data","Chabaudi_Burst_Sizediff.csv"))

subsetted_malaria_dat <- malaria_species_dat[,c("Immature_RBC_Burst_Size_Average",
                                                "Mature_RBC_Burst_Size_Average",
                                                "Infectivity_Preference",
                                                "Plasmodium.species")]



subsetted_malaria_dat2 <- na.omit(malaria_species_dat2[,c("Immature_BS_Average",
                                                 "Mature_BS_Average",
                                                 "Preference",
                                                 "Plasmodium_species")])

colnames(subsetted_malaria_dat2 ) <- c("Immature_Average", 
                                       "Mature_Average", 
                                       "Preference",
                                       "Species")
###I need to use the subsetted_malaria_dat_2 to add stuff into

subsetted_malaria_dat <- na.omit(subsetted_malaria_dat)

###Azurophilum is included so remove that
subsetted_malaria_dat <- subset(subsetted_malaria_dat,
                                subsetted_malaria_dat$Plasmodium.species != 'azurophilum')


colnames(subsetted_malaria_dat  ) <- c("Immature_Average", 
                                       "Mature_Average", 
                                       "Preference",
                                       "Species")



burst_size_Chabaudi$Preference = "All"
subsetted_chabaudi <- burst_size_Chabaudi[c(1:2),c("Retic","Mature", "Preference","Species")]
colnames(subsetted_chabaudi ) <-  c("Immature_Average", 
                                       "Mature_Average", 
                                       "Preference",
                                       "Species")

Full_Data_Preference <- rbind(subsetted_malaria_dat,
                              subsetted_malaria_dat2,
                              subsetted_chabaudi)


Full_Data_Preference [Full_Data_Preference $Preference=="UNKNOWN",]$Preference = "Unknown"



Full_Data_Preference$obs <- seq(1,nrow(Full_Data_Preference ))
Full_Data_Preference_Melted <- melt(Full_Data_Preference,
                                   id.vars=list("Species","Preference",'obs'))


Preference_GG <- ggplot(Full_Data_Preference_Melted, aes(x = variable, 
                           y= value,
                           group =obs))+
  geom_line(aes(linetype = Preference,
                linewidth = Preference,
                color = Preference))+
  geom_point(aes(color = Preference))+
  xlab("Age of RBC")+ ylab("Average burst size")+
  scale_x_discrete(label = c("Erythrocytes", "Normacytes"))+
  scale_linetype_manual(values=c("dashed","longdash", "dotted",  "solid"))+
  scale_color_manual(values= c("black","black","black","grey"))+
  scale_linewidth_manual(values = c(1.1,1.1,1.1,0.76))+
  theme_classic()+
  theme(axis.text = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 13, color = 'black'))




UPPER_GG + Preference_GG

ggsave(here("Figures", "Raw", "Duration_Age_preference.pdf"), units = 'in', 
       height = 5, width = 12)


