###CRISS_CROSS_PLOT

ALL_OPT <- read.csv(here("Output","ALL_OPT.csv"))

####################################################
Fitness_MODEL_PC <- read.csv(here(
  "Output", "Fitness_Model",
  "FITNESS_MODEL_PCdeathYes.csv"
))
Fitness_MODEL_PF <- read.csv(here(
  "Output", "Fitness_Model",
  "FITNESS_MODEL_PFdeathYes.csv"
))

###THESE HAVE TO DO WITH THE UN-CRISS-CROSSED FITNESS DATAFRAME

###Find the optimal C_V and B_V
PC_CV_optimal_CV <- Fitness_MODEL_PC[which.max(Fitness_MODEL_PC$end_fitness),]$C_V
PC_CV_optimal_BV <- Fitness_MODEL_PC[which.max(Fitness_MODEL_PC$end_fitness),]$B_V
PC_CV_optimal_fitness <- max(Fitness_MODEL_PC$end_fitness)

PC_CV_OPT_DF <- subset(Fitness_MODEL_PC , Fitness_MODEL_PC $C_V == PC_CV_optimal_CV  &  
                      Fitness_MODEL_PC $status == "success")

###Find the optimal C_V
PF_CV_optimal_CV <- Fitness_MODEL_PF[which.max(Fitness_MODEL_PF$end_fitness),]$C_V
PF_CV_optimal_BV <- Fitness_MODEL_PF[which.max(Fitness_MODEL_PF$end_fitness),]$B_V
PF_CV_optimal_fitness <- max(Fitness_MODEL_PF$end_fitness)

PF_CV_OPT_DF <-subset(Fitness_MODEL_PF , Fitness_MODEL_PF $C_V == PF_CV_optimal_CV  &  
                        Fitness_MODEL_PF $status == "success")

PC_Criss_Cross<- subset(ALL_OPT,ALL_OPT$species == "PC")

###Do it based on burst size change
PC_Criss_Cross$BV_percent_change <- ((PC_Criss_Cross$B_V-PC_CV_optimal_BV )/ PC_CV_optimal_BV)*100

PC_Criss_Cross$variable_interest <- factor(PC_Criss_Cross$variable_interest,
                                      levels =  PC_Criss_Cross$variable_interest[order(abs(PC_Criss_Cross$BV_percent_change), decreasing= TRUE)])

PC_Criss_Cross$Fitness_percent_change <- ((PC_Criss_Cross$end_fitness - PC_CV_optimal_fitness) / PC_CV_optimal_fitness) * 100


PC_Criss_Cross$variable_interest_2 <- PC_Criss_Cross$variable_interest
PC_Criss_Cross$variable_interest_2 <- factor(PC_Criss_Cross$variable_interest_2,
                                                  levels =  PC_Criss_Cross$variable_interest_2[order(abs(PC_Criss_Cross$Fitness_percent_change ), 
                                                                                                   decreasing= TRUE)])


###Plasmodium falciparum
PF_Criss_Cross <- subset(ALL_OPT,ALL_OPT$species == "PF")

PF_Criss_Cross$BV_percent_change <- (( PF_Criss_Cross$B_V - PF_CV_optimal_BV)/PF_CV_optimal_BV)*100
PF_Criss_Cross$variable_interest <- factor(PF_Criss_Cross$variable_interest,
                                           levels =  PF_Criss_Cross$variable_interest[order(abs(PF_Criss_Cross$BV_percent_change), decreasing= TRUE)])


PF_Criss_Cross$Fitness_percent_change <- ((PF_Criss_Cross$end_fitness -PF_CV_optimal_fitness)/
                                            PF_CV_optimal_fitness) * 100

PF_Criss_Cross$variable_interest_2 <- PF_Criss_Cross$variable_interest


PF_Criss_Cross$variable_interest_2<- factor(PF_Criss_Cross$variable_interest_2,
                                                  levels =  PF_Criss_Cross$variable_interest_2[order(abs(PF_Criss_Cross$Fitness_percent_change ), 
                                                                                                   decreasing= TRUE)])



PC_OPT_GG_BURST<- 
  ggplot(PC_Criss_Cross,
       aes(x = variable_interest, 
           y = B_V))+
  geom_hline(yintercept = PC_CV_optimal_BV, linetype = 2,color="#4B878BFF")+
  geom_hline(yintercept = PF_CV_optimal_BV , linetype = 2,color ="#D01C1FFF")+
  geom_segment(aes(x = variable_interest,
                   xend = variable_interest,
                   y = 15.5,
                   yend = B_V),
                   linewidth = 2,
                  color = "#4B878BFF")+
  geom_point(size = 4,
             color = "#4B878BFF")+
  xlab("Parasite traits")+
  ylab("Burst size (R)")+
  theme_classic()+
  ylim(0,50)+
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 13.5, color = 'black'))

PF_OPT_GG_BURST<- ggplot(PF_Criss_Cross,
                         aes(x = variable_interest,
                             y = B_V))+
  geom_hline(yintercept = PC_CV_optimal_BV, linetype = 2,color="#4B878BFF")+
  geom_hline(yintercept = PF_CV_optimal_BV , linetype = 2,color ="#D01C1FFF")+
  geom_segment(aes(x = variable_interest,
                   xend = variable_interest,
                   y = 17,
                   yend = B_V),
               
                color ="#D01C1FFF",
               linewidth = 2)+
  geom_point(size = 4,color = "#D01C1FFF")+
  xlab("Parasite traits")+
  ylab("Burst size")+
  ylim(0,50)+
  theme_classic()+
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 13.5, color = 'black'))


PC_OPT_GG_BURST | PF_OPT_GG_BURST

ggsave(here("Figures", "Raw", "Burst_Size_Cumulative_TP.pdf"), width = 10, height =4,
     units = 'in')


###


PC_OPT_GG_Fitness<- ggplot(PC_Criss_Cross,
                           aes(x = variable_interest_2, y =  
                                 Fitness_percent_change))+
 geom_point()+
  geom_segment(aes(x = variable_interest_2,
                   xend = variable_interest_2,
                   y = 0,
                   yend = Fitness_percent_change), 
               color ="#4B878BFF",
               linewidth = 2)+
  geom_hline(yintercept = 0)+
  geom_point(size = 4,color="#4B878BFF")+
  xlab("Parasite traits")+
  ylab("Cumulative \ntransmission potential")+
  theme_classic()+
  ylim(-55,55)+
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.text.y = element_text(size = 13.5, color = 'black'))


  PF_OPT_GG_Fitness<- ggplot(PF_Criss_Cross,
                           aes(x = variable_interest_2, y =
                                 Fitness_percent_change))+
  geom_point()+
    geom_hline(yintercept = 0)+
  geom_segment(aes(x = variable_interest_2,
                   xend = variable_interest_2, 
                   y = 0,
                   yend = Fitness_percent_change),
               color ="#D01C1FFF",
               linewidth = 2)+
  geom_point(size = 4,color ="#D01C1FFF")+
  xlab("Parasite traits")+
  ylab("Cumulative \ntransmission potential")+
  ylim (-55,55)+
  theme_classic()+
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.text.y = element_text(size = 13.5, color = 'black'))

PC_OPT_GG_Fitness|PF_OPT_GG_Fitness


ggsave(here("Figures", "Raw", "Burst_Size_Fitness_PCPF.pdf"),width = 10, height =4,
       units = 'in')
