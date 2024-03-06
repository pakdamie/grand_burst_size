###Why does increasing the length of the asexual cycle better?
SIM_PC_Unchanged <- Simulator_Malaria_BC_PC (15.5,0.76,4385.965,"Yes")
SIM_PC_Criss_Cross_alpha1 <- Simulator_PC_Criss_Cross("alpha1", 16,
                                                         0.76, 
                                                         0.58, 
                                                         NA,
                                                         4385.965,"Yes")


lookit <- FULL_MODEL_SIMULATING_Duration_criss_cross('alpha1',  4385.965, NA, 0.76,0.58, 
                                           NA, "PC", "Yes")


plot(SIM_PC_Criss_Cross_alpha1 $time, SIM_PC_Criss_Cross_alpha1  $G,type ='l', col = 'red',ylim=c(0,200000))
lines(SIM_PC_Unchanged$time, SIM_PC_Unchanged$G, type = 'l', ylim = c(0,200000))

plot(SIM_PC_Criss_Cross_alpha1[,'time'],PrI_PC(SIM_PC_Criss_Cross_alpha1[,"G"]),type='l',col='red')
lines(SIM_PC_Unchanged[,'time'],PrI_PC(SIM_PC_Unchanged[,"G"]),type='l')



abline(v = 45, col = 'red')










lines(SIM_PC[,'time'],RM_Calculator("PC",SIM_PC  ), col = '#4B878BFF',type = 'l', cex= 2, xlab = 'DPI',
     ylab = "RM")
lines(SIM_PF[,'time'], RM_Calculator("PF", SIM_PF), col = '#D01C1FFF',type = 'l', cex =2)
abline(h = 1)

###CRISS CROSS
alpha1_PC <- Simulator_PC_Criss_Cross("alpha1",15.5, 0.76, 4385.965)
alpha1_PF <- Simulator_PF_Criss_Cross("alpha1",20.25, 0.56, 25000)

lines(SIM_PF[,'time'], SIM_PF[,'R'],type = 'l')
plot(alpha1_PF[,'time'],alpha1_PF[,'R'], type = 'l', col = 'red')

plot(alpha1_PF [,'time'], RM_Calculator_Criss_Cross("PF", alpha1_PF, "alpha1"), col = '#D01C1FFF',type = 'l', 
     xlab = 'DPI',ylab = "RM",lty =2)
lines(SIM_PF[,'time'], RM_Calculator("PF", SIM_PF), col = '#D01C1FFF',type = 'l', cex =2)
lines(SIM_PC[,'time'],RM_Calculator("PC",SIM_PC  ), col = '#4B878BFF',type = 'l', cex= 2, xlab = 'DPI',
         ylab = "RM")

plot(alpha1_PC [,'time'], RM_Calculator_Criss_Cross("PC", alpha1_PC, "alpha1"), col = '#4B878BFF',type = 'l', 
     xlab = 'DPI',ylab = "RM",lty =2, ylim = c(0,3))
lines(SIM_PC[,'time'], RM_Calculator("PC", SIM_PC), col = '#4B878BFF',type = 'l', cex =2)
lines(SIM_PC[,'time'],RM_Calculator("PF",SIM_PF  ), col = '#D01C1FFF',type = 'l', cex= 2, xlab = 'DPI',
      ylab = "RM")




###CRISS CROSS
muM_PC <- Simulator_PC_Criss_Cross("muM", 15.5, 0.76, 4385.965)
muM_PF <- Simulator_PF_Criss_Cross(c("muM"), 20.25, 0.56, 25000)




lines(SIM_PF[,'time'], SIM_PF[,'R'],type = 'l')
plot(alpha1_PF[,'time'],alpha1_PF[,'R'], type = 'l', col = 'red')

plot(muM_PF [,'time'], RM_Calculator_Criss_Cross("PF", muM_PF, "muM"), col = '#D01C1FFF',type = 'l', 
     xlab = 'DPI',ylab = "RM",lty =2)
lines(SIM_PF[,'time'], RM_Calculator("PF", SIM_PF), col = '#D01C1FFF',type = 'l', cex =2)
lines(SIM_PC[,'time'],RM_Calculator("PC",SIM_PC  ), col = '#4B878BFF',type = 'l', cex= 2, xlab = 'DPI',
      ylab = "RM")

plot(muM_PC[,'time'], RM_Calculator_Criss_Cross("PC", muM_PC , "muM"), col = '#4B878BFF',type = 'l', 
     xlab = 'DPI',ylab = "RM",lty =2, ylim = c(0,3))
lines(SIM_PC[,'time'], RM_Calculator("PC", SIM_PC), col = '#4B878BFF',type = 'l', cex =2)
lines(SIM_PC[,'time'],RM_Calculator("PF",SIM_PF  ), col = '#D01C1FFF',type = 'l', cex= 2, xlab = 'DPI',
      ylab = "RM")

###DOUBLE


alpha1_muM <- Simulator_PF_Criss_Cross_DOUBLE_TROUBLE(15.5, 0.56, 25000)
plot(alpha1_muM[,'time'],RM_Calculator_Criss_Cross_DOUBLE(alpha1_muM))

