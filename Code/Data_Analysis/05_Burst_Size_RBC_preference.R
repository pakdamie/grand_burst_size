###This script is to plot the facet and show that there is
### a lot of confusion when it comes to talk about RBC tropism.

library(here)
source(here("Code","Functions","FUNC_full_data_loader.R"))

###I need to use the subsetted_malaria_dat_2 to add stuff into
subsetted_preference_dat <- mal_dat_asex_full[,c("Plasmodium.species","Type_Host",
                                                 "Known_Preference","Inferred_Preference",
                                                 "Higher.burst.size.in", "Infectivity_note")]

###Get rid of circumflexum and lophurae



###Get only the data where there is some information about the preference
subsetted_preference_dat <- subset(subsetted_preference_dat,
                                  is.na(subsetted_preference_dat$Known_Preference) == FALSE &
                                    !(subsetted_preference_dat$Plasmodium.species %in% c("lophurae_2", "circumflexum_2")))

pruned.tree <-drop.tip(Full_SuperTree_Host,
                       setdiff(Full_SuperTree_Host$tip.label, 
                        unique(subsetted_preference_dat$Type_Host)))


pruned.tree_host_order <- cbind.data.frame(order = seq(1,length(pruned.tree $tip.label)),Type_Host = pruned.tree $tip.label)

ordered_pruned_dat <- left_join(subsetted_preference_dat , pruned.tree_host_order )

ordered_pruned_dat$Plasmodium.species <- factor(ordered_pruned_dat$Plasmodium.species ,
                                                   levels = c(ordered_pruned_dat$Plasmodium.species[order(ordered_pruned_dat$order)]))
                                                   
ordered_pruned_dat$facet_var <-ifelse(ordered_pruned_dat$order > 33, 2,1)



ordered_pruned_dat$point1 <- NA
ordered_pruned_dat$point2 <- NA
ordered_pruned_dat$colortriangle1 <- NA
ordered_pruned_dat$colortriangle2 <- NA



only_conflict_df <-  subset(ordered_pruned_dat, ordered_pruned_dat$Known_Preference %in% c("Conflict(All,Immature)*",
                                                                                           "Conflict(All,Mature)*", "Conflict(All,Mature)",
                                                                                           "Conflict(All;Mature)",
                                                                                           "Conflict(Immature,Mature)*", 
                                                                                           "Conflict(Immature,Mature)") |
                              ordered_pruned_dat$Inferred_Preference %in% c("Conflict(All,Immature)*",
                                                                         "Conflict(All,Mature)*", "Conflict(All,Mature)",
                                                                         "Conflict(Immature,Mature)*", 
                                                                         "Conflict(Immature,Mature)") )
non_conflict_df <- subset(ordered_pruned_dat, !(ordered_pruned_dat$Plasmodium.species %in% only_conflict_df$Plasmodium.species))
  
  




meltedconflict_df <- melt(only_conflict_df[,-6], id.vars = c("Plasmodium.species", "Type_Host", "order", "facet_var"))
meltedconflict_df$point1 <- "Square"
meltedconflict_df$point2 <- "Square"
meltedconflict_df$color1 <- NA
meltedconflict_df$color2 <- NA


meltedconflict_df[meltedconflict_df$value == "Conflict(All,Immature)*",]$point1 <- "a"
meltedconflict_df[meltedconflict_df$value == "Conflict(All,Immature)*",]$color1 <- "Immature"

meltedconflict_df[meltedconflict_df$value == "Conflict(All,Immature)*",]$point2 <- "b"
meltedconflict_df[meltedconflict_df$value == "Conflict(All,Immature)*",]$color2 <- "All"

meltedconflict_df[meltedconflict_df$value %in% c("Conflict(All,Mature)*", "Conflict(All,Mature)"),]$point1 <- "a"
meltedconflict_df[meltedconflict_df$value %in% c("Conflict(All,Mature)*", "Conflict(All,Mature)"),]$color1 <- "Mature"

meltedconflict_df[meltedconflict_df$value %in% c("Conflict(All,Mature)*", "Conflict(All,Mature)"),]$point2 <- "b"
meltedconflict_df[meltedconflict_df$value %in% c("Conflict(All,Mature)*", "Conflict(All,Mature)"),]$color2 <- "All"


meltedconflict_df[meltedconflict_df$value %in% c("Conflict(Immature,Mature)*", "Conflict(Immature,Mature)"),]$point1 <- "a"
meltedconflict_df[meltedconflict_df$value %in% c("Conflict(Immature,Mature)*", "Conflict(Immature,Mature)"),]$color1 <- "Immature"

meltedconflict_df[meltedconflict_df$value %in% c("Conflict(Immature,Mature)*", "Conflict(Immature,Mature)"),]$point2 <- "b"
meltedconflict_df[meltedconflict_df$value %in% c("Conflict(Immature,Mature)*", "Conflict(Immature,Mature)"),]$color2 <- "Mature"


meltedconflict_df[meltedconflict_df$value %in% c("Immature"),]$color1 <- 'Immature' 
meltedconflict_df[meltedconflict_df$value %in% c("Mature"),]$color1 <- 'Mature' 
meltedconflict_df[meltedconflict_df$value %in% c("Unknown"),]$color1 <- 'Unknown' 


meltednonconflict_df <- melt(non_conflict_df[,-6], id.vars = c("Plasmodium.species", "Type_Host", "order", "facet_var"))
meltednonconflict_df$point1 <- "Square"
meltednonconflict_df$point2 <- "Square"
meltednonconflict_df$color1 <- NA
meltednonconflict_df$color2 <- NA

meltednonconflict_df[meltednonconflict_df$value %in% c("Immature", "Immature*"),]$color1 <- 'Immature' 
meltednonconflict_df[meltednonconflict_df$value %in% c("Mature","Mature*"),]$color1 <- 'Mature' 
meltednonconflict_df[meltednonconflict_df$value %in% c("All,All*"),]$color1 <- 'All'
meltednonconflict_df[meltednonconflict_df$value %in% c("Unknown",NA),]$color1 <- 'Unknown'
meltednonconflict_df[meltednonconflict_df$value %in% c("No"),]$color1 <- "No"
  

squared_melted_conflict <- subset(meltedconflict_df,meltedconflict_df$point1 == "Square")
full_melted_df<- rbind.data.frame(squared_melted_conflict,meltednonconflict_df)
full_melted_df[is.na(full_melted_df$color1) == TRUE,]$color1 <- 'Unknown'

ggplot(subset(full_melted_df,full_melted_df$facet_var == 1),
              aes(x = variable, 
                  y = Plasmodium.species)) +
                  geom_tile(aes(fill = color1),color= 'black', size =0.5) + 
  geom_point(data = subset(meltedconflict_df,meltedconflict_df$facet_var ==1 &meltedconflict_df$point1 != "Square"),
                           aes(x = variable, y= Plasmodium.species, shape = point1,color = color1), size = 13.5)+
  geom_point(data = subset(meltedconflict_df,meltedconflict_df$facet_var ==1 &meltedconflict_df$point1 != "Square"  ),aes(x = variable, y = Plasmodium.species, 
                                                                                  shape = point2, color = color2), size = 13.5)+
  scale_shape_manual(values=c("\u25E4","\u25E2"))+
  scale_color_manual(values = c("All" = "#f16362", "Immature" = "#003f5b", "Mature" = '#faa51a')) +
  scale_fill_manual(values = c(
                               "Immature" = '#003f5b',
                               "Mature" = '#faa51a', 
                               "Unknown" = "lightgrey", "No" = "black"),na.value = 'black')+
  scale_x_discrete(label = c("Known preference", "Inferred preference", "Greater burst size in..."))+
  xlab("")+
  ylab("")+
  coord_equal()+
  geom_vline(xintercept =c(1.5,1 + 1.5), size = 0.7)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none',
        axis.line = element_blank(),
        axis.text = element_text(color = 'black', size = 14))
  

ggplot(subset(full_melted_df,full_melted_df$facet_var == 2),
       aes(x = variable, 
           y = Plasmodium.species)) +
  geom_tile(aes(fill = color1),color= 'black', size =0.5) + 
  geom_point(data = subset(meltedconflict_df,meltedconflict_df$facet_var ==2 &meltedconflict_df$point1 != "Square"),
             aes(x = variable, y= Plasmodium.species, shape = point1,color = color1), size = 16.5)+
  geom_point(data = subset(meltedconflict_df,meltedconflict_df$facet_var ==2 &meltedconflict_df$point1 != "Square"  ),aes(x = variable, y = Plasmodium.species, 
                                                                                                                          shape = point2, color = color2), size = 16.5)+
  scale_shape_manual(values=c("\u25E4","\u25E2"))+
  scale_color_manual(values = c("All" = "#f16362", "Immature" = "#003f5b", "Mature" = '#faa51a')) +
  scale_fill_manual(values = c(
    "Immature" = '#003f5b',
    "Mature" = '#faa51a', 
    "Unknown" = "lightgrey", "No" = "black"),na.value = 'black')+
  scale_x_discrete(label = c("Known preference", "Inferred preference", "Greater burst size in..."))+
  xlab("")+
  ylab("")+
  geom_vline(xintercept =c(1.5,1 + 1.5), size = 0.7)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none',
        axis.line = element_blank(),
        axis.text = element_text(color = 'black', size = 14))


a<- ggplot(subset(full_melted_df,full_melted_df$facet_var == 1),
       aes(y = variable, 
           x = Plasmodium.species)) +
  geom_tile(aes(fill = color1),color= 'black', 
            size =0.5) + 
  geom_point(data = subset(meltedconflict_df,meltedconflict_df$facet_var ==1 &meltedconflict_df$point1 != "Square"),
             aes(y = variable, x= Plasmodium.species, shape = point1,color = color1), size = 30.5)+
  geom_point(data = subset(meltedconflict_df,meltedconflict_df$facet_var ==1 &meltedconflict_df$point1 != "Square"  ),aes(y= variable,x = Plasmodium.species, 
                                                                                                                          shape = point2, color = color2), size = 30.5)+
  scale_shape_manual(values=c("\u25E4","\u25E2"))+
  scale_color_manual(values = c("All" = "#f16362", "Immature" = "#003f5b", "Mature" = '#faa51a')) +
  scale_fill_manual(values = c(
    "Immature" = '#003f5b',
    "Mature" = '#faa51a', 
    "Unknown" = "lightgrey", "No" = "black"),na.value = 'black')+
  scale_y_discrete(label = c("Known preference", "Inferred preference", "Greater burst size in..."),)+
  xlab("")+
  ylab("")+
  coord_equal()+
  theme_classic()+
  geom_hline(yintercept =c(1.5,1 + 1.5), size = 0.7)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none',
        axis.line = element_blank(),
        axis.text = element_text(color = 'black', size = 14))


b<- ggplot(subset(full_melted_df,full_melted_df$facet_var == 2),
       aes(y = variable, 
           x = Plasmodium.species)) +
  geom_tile(aes(fill = color1),color= 'black', 
            size =0.5) + 
  geom_point(data = subset(meltedconflict_df,meltedconflict_df$facet_var ==2 &meltedconflict_df$point1 != "Square"),
             aes(y = variable, x= Plasmodium.species, shape = point1,color = color1), size = 10)+
  geom_point(data = subset(meltedconflict_df,meltedconflict_df$facet_var ==2 &meltedconflict_df$point1 != "Square"  ),aes(y= variable,x = Plasmodium.species, 
                                                                                                                          shape = point2, color = color2), size = 10)+
  scale_shape_manual(values=c("\u25E4","\u25E2"))+
  scale_color_manual(values = c("All" = "#f16362", "Immature" = "#003f5b", "Mature" = '#faa51a')) +
  scale_fill_manual(values = c(
    "Immature" = '#003f5b',
    "Mature" = '#faa51a', 
    "Unknown" = "lightgrey", "No" = "black"),na.value = 'black')+
  scale_y_discrete(label = c("Known preference", "Inferred preference", "Greater burst size in..."),)+
  xlab("")+
  ylab("")+
  coord_equal()+
  theme_classic()+
  geom_hline(yintercept =c(1.5,1 + 1.5), size = 0.7)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none',
        axis.line = element_blank(),
        axis.text = element_text(color = 'black', size = 14))

a/b
