###This is to make the schematic figure to show
###the difference between the P. falciparum and P. chabaudi 

pmax_PCPF = c(PC = 4.0e-6, PF = 8.35e-6, variable = "Invasion rate")
alpha1_PCPF <- c(PF= "1 day" , PF =  "2 day", variable = "Asexual cycle (days)") #EXPRESS IN DAYS 
alpha2_PCPF <- c(PC = "2 day", PF =  "7 day" ,variable = "Sexual cycle (days)") #EXPRESS IN DAYS
muM_PCPF <- c(PC = "30 min", PF =  "7.2 min" , variable = "Merozoite lifespan (minutes)" ) #EXPRESS IN MINUTES
muG_PCPF <- c(PC = "0.25 day", PF =   "3.5 day", variable = "Gametocyte lifespan (days)") #EXPRESS IN Days
cv_PCPF <- c(PC = 0.76, PF = 0.48, variable = "Optimal transmission investment" )

FULL_PCPF <- rbind.data.frame(pmax_PCPF,alpha1_PCPF,alpha2_PCPF,muM_PCPF,muG_PCPF,cv_PCPF)
colnames(FULL_PCPF) <- c("P.c", "P.f", "variable")




FULL_PCPF <- melt(FULL_PCPF, id.vars = 'variable')
colnames(FULL_PCPF) <- c("variable", "species", 'value')

FULL_PCPF_GG <- ggplot(FULL_PCPF, aes(x = species, y =(value), group =variable))+ 
  geom_line()+ facet_wrap(~variable, scales = 'free',ncol = 2)+
  geom_point(size =5, aes(color = species)) + 
  scale_color_manual(values = c("#4B878BFF","#D01C1FFF"))+
  xlab("")+
  ylab("")+
  theme_classic()+
  theme(strip.background = element_blank(),
        axis.text = element_text(size = 14, color = 'black'),
        strip.text = element_text(size = 15,color = 'black'))

ggsave(here("Figures", "Raw", "FULL_PCPF_GG.pdf"),
            width = 9, height = 10, units = 'in')
