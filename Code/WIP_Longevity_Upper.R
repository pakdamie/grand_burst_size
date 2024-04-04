

data(age)


Bird_age_data <- subset(age, age$class == "Aves")
Bird_age_data$ genus_species <- paste(Bird_age_data $genus,
                                      Bird_age_data$species,
                                      sep ="_")

Avian_Merged_F_Mass <- left_join(Avian_MERGED_F,
          Bird_age_data, 
          genus_species,
          by =join_by("Species" == "genus_species"))


Avian_Merged_F_Mass_NAomit <- Avian_Merged_F_Mass[is.na(Avian_Merged_F_Mass$hagrid) == FALSE,]
row.names(Avian_Merged_F_Mass_NAomit) <- Avian_Merged_F_Mass_NAomit $Species
consensus_AVE_Tree_AnAge <- keep.tip(consensus_AVE_Tree, Avian_Merged_F_Mass_NAomit $Species)

Avian_Merged_F_Mass_NAomit_max <- Avian_Merged_F_Mass_NAomit[is.na(Avian_Merged_F_Mass_NAomit$maximum_longevity_yr)
                                                             == FALSE,]


Avian_Merged_F_Mass_NAomit_max <- Avian_Merged_F_Mass_NAomit_max[,c("Upper","maximum_longevity_yr","Species")]

Avian_Data_Merged_Phylogeny <- phylo4d(
  keep.tip(consensus_AVE_Tree,  Avian_Merged_F_Mass_NAomit_max$Species),
  tip.data =  
    Avian_Merged_F_Mass_NAomit_max[,c(1,2)],
  match.data = TRUE
)

Avian_Dat_ABOUHEIF.MORAN <- abouheif.moran(
  Avian_Data_Merged_Phylogeny , nrepet = 5000)



a<- ggplot(data = 
         Avian_Merged_F_Mass_NAomit , 
        aes(x =(maximum_longevity_yr), 
            y= (Upper)))+geom_point(color = 'red')

b<-  ggplot(data = 
               Data_Mammal_Mass_Retic,
             aes(x =(max_longevity_d/365),
                 y = (Upper)), color = 'blue')+
  geom_point()
  
b<-ggplot(data = 
         Avian_Merged_F_Mass_NAomit , 
       aes(x =log(adult_mass_g), 
           y= log(Upper)))+geom_point(color = 'red')

  geom_point(data = 
               Data_Mammal_Mass_Retic,
             aes(x = log(adult_mass_g),
                 y = log(Upper)), color = 'blue')


bird_dat <- Avian_Merged_F_Mass_NAomit[,c("adult_mass_g","maximum_longevity_yr","Upper","Species")]
mam_dat <-  Data_Mammal_Mass_Retic[,c("adult_mass_g","max_longevity_d","Upper","Species")]
colnames(mam_dat)[2] <- 'maximum_longevity_yr'
mam_dat$maximum_longevity_yr <- mam_dat$maximum_longevity_yr/365

full_dat <- rbind(bird_dat, mam_dat)

summary(glm(Upper~log(adult_mass_g), data=full_dat, family = 'poisson'))
summary(glm(Upper~log(maximum_longevity_yr), data=full_dat, family = 'poisson'))




Rep_age_data <- subset(age, age$class == "Reptilia")
Rep_age_data$ genus_species <- paste(Rep_age_data$genus,
                                     Rep_age_data$species,
                                      sep ="_")

REP_Merged_F_Mass <- left_join(Reptile_MERGED_F,
                                 Rep_age_data, 
                                 genus_species,
                                 by =join_by("Species" == "genus_species"))



Rep_Merged_F_Mass_NAomit <- REP_Merged_F_Mass[is.na(REP_Merged_F_Mass$hagrid) == FALSE,]
row.names(Rep_Merged_F_Mass_NAomit) <- Rep_Merged_F_Mass_NAomit $Species
consensus_REP_Tree_AnAge <- keep.tip(consensus_REP_Tree, Rep_Merged_F_Mass_NAomit $Species)
