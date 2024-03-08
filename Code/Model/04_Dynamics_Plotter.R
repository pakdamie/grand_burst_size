###Why does increasing the length of the asexual cycle better for cumulative fitness? (PC)
SIM_PC_UNCHANGED <- Single_BV_CV_Simulator(15.5,0.76,"NO","PC",4385.965, NA)
SIM_PC_alpha1 <- Single_BV_CV_Simulator(15.5,0.76,"YES","PC",4385.965, "alpha1")

FULL_PC_alpha1_GG <- ggplot(subset(SIM_PC_UNCHANGED,SIM_PC_UNCHANGED$time < 50) , 
                            aes(x = time, y = prob))+ 
  geom_line(color = "#4B878BFF")+
  geom_line(data = subset(SIM_PC_alpha1,SIM_PC_alpha1$time < 70), 
            aes(x = time, y=prob), col = '#4B878BFF', linetype = 2) +
  theme_bw()+
  xlab("Days post-infection")+
  ylab("Transmission probability")+
  ggtitle("Asexual cycle length is increased")+
  theme_classic()+
  theme(axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'),
        plot.title = element_text(size = 16))

FULL_PC_alpha1_GG 

###Why does increasing the length of the asexual cycle worse for cumulative fitness? (PF)

SIM_PF_UNCHANGED <- Single_BV_CV_Simulator(17,0.48,"NO","PF",25000, NA)
SIM_PF_alpha1 <- Single_BV_CV_Simulator(17,0.48,"YES","PF",25000, "alpha1")

FULL_PF_alpha1_GG <- ggplot(subset(SIM_PF_UNCHANGED,
                                   SIM_PF_UNCHANGED$time < 100) , 
                            aes(x = time, y = prob))+ 
  geom_line(color = '#D01C1FFF')+
  geom_line(data = subset(SIM_PF_alpha1,SIM_PF_alpha1$time < 100), 
            aes(x = time, y=prob), col = '#D01C1FFF', linetype = 2) +
  theme_bw()+
  xlab("Days post-infection")+
  ylab("Transmission probability")+
  ggtitle("Asexual cycle length is decreased")+
  theme_classic()+
  theme(axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'),
        plot.title = element_text(size = 16))

FULL_PF_alpha1_GG 

