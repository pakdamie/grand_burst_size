###CRISS_CROSS_PLOT

ALL_OPT <- read.csv(here("Output","ALL_OPT.csv"))

Fitness_MODEL_PC <- read.csv(here(
  "Output", "Fitness_Model",
  "FITNESS_MODEL_PC.csv"
))
Fitness_MODEL_PF <- read.csv(here(
  "Output", "Fitness_Model",
  "FITNESS_MODEL_PF.csv"
))

 
###THESE HAVE TO DO WITH THE UN-CRISS-CROSSED FITNESS DATAFRAME

###Find the optimal C_V and B_V
PC_CV_optimal_CV <- Fitness_MODEL_PC[which.max(Fitness_MODEL_PC$end_fitness),]$C_V
PC_CV_optimal_BV <- Fitness_MODEL_PC[which.max(Fitness_MODEL_PC$end_fitness),]$B_V

PC_CV_OPT_DF <- subset(Fitness_MODEL_PC , Fitness_MODEL_PC $C_V == PC_CV_optimal_CV  &  
                      Fitness_MODEL_PC $status == "success")

###Find the optimal C_V
PF_CV_optimal_CV <- Fitness_MODEL_PF[which.max(Fitness_MODEL_PF$end_fitness),]$C_V
PF_CV_optimal_BV <- sqrt(Fitness_MODEL_PF[which.max(Fitness_MODEL_PF$end_fitness),]$B_V)

PF_CV_OPT_DF <-subset(Fitness_MODEL_PF , Fitness_MODEL_PF $C_V == PF_CV_optimal_CV  &  
                        Fitness_MODEL_PF $status == "success")

PC_Criss_Cross<- subset(ALL_OPT,ALL_OPT$species == "PC")





PC_OPT$fitness_percent_change <- ((PC_CV_OPT_Best$end_fitness-PC_OPT$end_fitness)/PC_OPT$end_fitness)*100
PC_OPT$variable_interest <- factor(PC_OPT$variable_interest,
                                      levels =  PC_OPT$variable_interest[order(abs(PC_OPT$fitness_percent_change), decreasing= TRUE)])

PC_OPT$change_fitness <- ifelse(PC_OPT$end_fitness > PC_CV_OPT_Best$end_fitness, "Increase","Decrease")
PC_OPT$burst_change <- ifelse(PC_OPT$B_V > PC_CV_OPT_Best$R, "Increase","Decrease")



PF_OPT <- subset(ALL_OPT,ALL_OPT$species == "PF")
PF_OPT$fitness_percent_change <- ((PF_CV_OPT_Best$end_fitness-PF_OPT$end_fitness)/PF_OPT$end_fitness)*100
PF_OPT$variable_interest <- factor(PF_OPT$variable_interest,
                                   levels =  PF_OPT$variable_interest[order(abs(PF_OPT$fitness_percent_change), decreasing= TRUE)])

PF_OPT$change_fitness <- ifelse(PF_OPT$end_fitness > PF_CV_OPT_Best$end_fitness, "Increase","Decrease")
PF_OPT$burst_change <- ifelse(PF_OPT$B_V > PF_CV_OPT_Best$R, "Increase","Decrease")




PC_OPT_GG_BURST<- 
  ggplot(PC_OPT,
       aes(x = variable_interest, 
           y = B_V))+
  geom_hline(data = PC_CV_OPT_Best, aes(yintercept = R), linetype = 2,color="#4B878BFF")+
  geom_hline(data= PF_CV_OPT_Best, aes(yintercept = B_V), linetype = 2,color ="#D01C1FFF")+
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
  ylim(0,90)+
  theme_classic()+
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 13.5, color = 'black'))



PC_OPT_GG_Fitness<- ggplot(PC_OPT,
                   aes(x = variable_interest, y =end_fitness ))+
  geom_hline(data = PC_CV_OPT_Best, aes(yintercept = end_fitness), linetype = 2,color="#4B878BFF")+
  geom_hline(data= PF_CV_OPT_Best, aes(yintercept = end_fitness), linetype = 2,color ="#D01C1FFF")+
  geom_segment(aes(x = variable_interest,
                   xend = variable_interest,
                   y = 27.54737,
                   yend =end_fitness) , 
               color ="#4B878BFF",
               linewidth = 2)+
  geom_point(size = 4,color="#4B878BFF")+
  xlab("Parasite traits")+
  ylab("Cumulative \ntransmission potential")+
  theme_classic()+
  ylim (20,60)+
  
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.text.y = element_text(size = 13.5, color = 'black'))

PC_OPT_GG_BURST /PC_OPT_GG_Fitness



PF_OPT_GG_BURST<- ggplot(PF_OPT,
                         aes(x = variable_interest,
                             y = B_V))+
  geom_hline(data = PC_CV_OPT_Best, aes(yintercept = R), linetype = 2,color="#4B878BFF")+
  geom_hline(data= PF_CV_OPT_Best, aes(yintercept = B_V), linetype = 2,color ="#D01C1FFF")+
  geom_segment(aes(x = variable_interest,
                   xend = variable_interest,
                   y = 42.5,
                   yend = B_V),
               
                color ="#D01C1FFF",
               linewidth = 2)+
  geom_point(size = 4,color = "#D01C1FFF")+
  xlab("Parasite traits")+
  ylab("Burst size")+
  ylim(0,90)+
  theme_classic()+
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 13.5, color = 'black'))



PF_OPT_GG_Fitness<- ggplot(PF_OPT,
                           aes(x = variable_interest, y =end_fitness ))+
  geom_hline(data = PC_CV_OPT_Best, aes(yintercept = end_fitness), linetype = 2,color="#4B878BFF")+
  geom_hline(data= PF_CV_OPT_Best, aes(yintercept = end_fitness), linetype = 2,color ="#D01C1FFF")+
  geom_segment(aes(x = variable_interest,
                   xend = variable_interest,
                   y = 50.88641,
                   yend =end_fitness),
               color ="#D01C1FFF",
               linewidth = 2)+
  geom_point(size = 4,color ="#D01C1FFF")+
  xlab("Parasite traits")+
  ylab("Cumulative \ntransmission potential")+
  theme_classic()+
  ylim (20,60)+
  theme(axis.title = element_text(size = 14, color = 'black'),
        axis.text.y = element_text(size = 13.5, color = 'black'))


(PC_OPT_GG_BURST /PC_OPT_GG_Fitness) | (PF_OPT_GG_BURST/PF_OPT_GG_Fitness)

ggsave(here("Figures", "Raw", "Burst_Size_Cumulative_TP.pdf"), width = 7, height =8,
     units = 'in')
