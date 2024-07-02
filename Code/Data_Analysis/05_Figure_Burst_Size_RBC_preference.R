### This script is to plot the facet and show that there is
### a lot of confusion when it comes to talking about RBC tropism.
### Run this script when you want to get the facet for Figure 3.

library(here)
source(here("Code", "Functions", "FUNC_DA_full_data_loader.R"))

### Subsetted from the main_dat
subsetted_preference_dat <- mal_dat_asex_full[, c(
  "Plasmodium.species", "Type_Host","OrderHost",
  "Known_Preference", "Inferred_Preference",
  "Higher.burst.size.in", "Infectivity_note", "Lower","Upper"
)]

### Get rid of circumflexum and lophurae AND
### Get only the data where there is some information about the preference
subsetted_preference_dat <- subset(
  subsetted_preference_dat,
  is.na(subsetted_preference_dat$Known_Preference) == FALSE &
    !(subsetted_preference_dat$Plasmodium.species %in% c("lophurae_2", "circumflexum_2"))
)

subsetted_preference_known <- 
  subset(subsetted_preference_dat, !(subsetted_preference_dat$Known_Preference) == "Unknown")

subsetted_preference_known$Known_Preference[subsetted_preference_known$Known_Preference == "All*"] = "All"
subsetted_preference_known$Known_Preference[subsetted_preference_known$Known_Preference == "All*"] = "All"
subsetted_preference_known$
  Known_Preference[subsetted_preference_known$Known_Preference == "Conflict(All,Mature)*"] = "Conflict(All;Mature)"

subsetted_preference_known$
  Known_Preference[subsetted_preference_known$Known_Preference == "Conflict(Immature,Mature)*"] = "Conflict(Immature,Mature)"

subsetted_preference_known$
  Known_Preference[subsetted_preference_known$Known_Preference == "Mature*"] = "Mature"

subsetted_preference_known$Conflict <- "None"

subsetted_preference_known$Conflict[subsetted_preference_known$Known_Preference == "Conflict(All;Mature)"] = "Mature"
subsetted_preference_known$Conflict[subsetted_preference_known$Known_Preference == "Conflict(All,Immature)*"] = "Immature"


subsetted_preference_known$Known_Preference[subsetted_preference_known$Known_Preference == "Conflict(All;Mature)"] = "All"
subsetted_preference_known$Known_Preference[subsetted_preference_known$Known_Preference == "Conflict(All,Immature)*"] = "All"

subsetted_preference_known$Known_Preference<- factor(subsetted_preference_known$Known_Preference,
       levels=c("Immature","Conflict(Immature,Mature)","Mature", "All"))


ggplot(
  subsetted_preference_known,
  aes(x = Known_Preference, y = Upper)) + 
  geom_point(aes(shape = Conflict,color = OrderHost),size = 5, stroke = 1.2
              )+
  xlab("Known preference")+
  ylab("Maximum observed burst size")+
  scale_shape_manual(values = c(2,3,1))+
  scale_color_manual(values = c("avian" =   "#30d5c8" ,
                                "mammal" = "#fe64a3", 
                                "reptile" = "#424b54"),
                     label = c("avian" = "Avian",
                               "mammal" = "Mammal",
                               "reptile" = "Squamatian"))+
  theme_classic()+
  theme(axis.text = element_text(size = 12, color = 'black'),
        axis.title = element_text(size = 14, color = 'black'))
    
ggsave(here("Figures", "Raw", "Agepreference_Figure.pdf"),
       height = 5,
       width = 8, units = "in"
)
