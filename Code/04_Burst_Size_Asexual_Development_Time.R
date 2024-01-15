###Figure making script that looks at the 
###asexual development time and the maximum burst size to see if
###there is any relationship between them.
###This is the plot making figure

library(here)
library(ggplot2)
library(ggbeeswarm)
###Malaria Data - Species
mal_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

###Subset the data
subsetted_mal_dat <-   mal_dat[,c("Plasmodium.species",
                                  "Average",
                                  "Lower",
                                  "Upper",
                                  "Duration",
                                  "OrderHost")]

###Make sure I'm not getting any data that has missing duration data
subsetted_mal_dat <- subsetted_mal_dat[is.na(subsetted_mal_dat $Duration) == FALSE 
                                       & subsetted_mal_dat $Duration != "",]

###Circumflexum and Lophurae have different durations that I need to account for
subsetted_mal_dat[subsetted_mal_dat$Plasmodium.species == 'circumflexum',]$Plasmodium.species <- "circumflexum_1"

subsetted_mal_dat[subsetted_mal_dat$Plasmodium.species == 'circumflexum_1',]$Duration <- 24

#circumflexum_2 
circ_2 <- data.frame("Plasmodium.species" = "circumflexum_2",
           "Average" = 16,
           "Lower" = 8,
           "Upper" = 30,
           "Duration" = 48,
           "OrderHost" = 'avian')
  
###24/36
subsetted_mal_dat[subsetted_mal_dat$Plasmodium.species == 'lophurae',]$Plasmodium.species<- "lophurae_1"

subsetted_mal_dat[subsetted_mal_dat$Plasmodium.species == 'lophurae_1',]$Duration <- 24


#lophurae_2 
loph_2 <- data.frame("Plasmodium.species" = "lophurae_2",
                     "Average" = 13,
                     "Lower" = 8,
                     "Upper" = 18,
                     "Duration" = 36,
                     "OrderHost" = 'avian')

###This is the full data that you want to use.
Full_Mal_Duration_Data<-
      rbind(subsetted_mal_dat,
      circ_2,
      loph_2)

Full_Mal_Duration_Data$Duration <- as.numeric(Full_Mal_Duration_Data$Duration)
Full_Mal_Duration_Data$Upper <- as.numeric(Full_Mal_Duration_Data$Upper)



UPPER_GG  <- ggplot(Full_Mal_Duration_Data, 
                    aes(x = (Duration), 
                        y = Upper, 
                        fill = as.factor(Duration)))+
  geom_beeswarm(size = 3, shape = 21)+
  scale_fill_viridis(option = 'viridis', discrete = TRUE)+
  scale_x_continuous(breaks = c(0,24,48, 72, 96))+
  scale_y_continuous(limits = c(0,40), 
                     breaks = seq(0,40,5))+
  xlab("Duration (Hours)") +
  ylab("Upper burst size") +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))

UPPER_GG


ggsave(here("Figure","Data_Analysis","Burst_Size_AesxualDuration_Figure.pdf"),
       width = 10, height = 4, units = 'in')


###SUPPLEMENT

LOWER_GG  <- ggplot(Full_Mal_Duration_Data, 
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


###Statistical test to see if there is a relationship

LOWER_LM_GLM <- glm.nb(Lower ~ Duration,  data = Full_Mal_Duration_Data)
summary(LOWER_LM_GLM)
testDispersion(LOWER_LM_GLM) #using the neg binomial distribution does
                             #not have the overdispersion problem

simulationOutput <- simulateResiduals(fittedModel = LOWER_LM_GLM, plot = T)
residuals(simulationOutput)
plot(simulationOutput)


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
