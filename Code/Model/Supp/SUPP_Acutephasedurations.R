###Acute phase durations for Plasmodium chabaudi and falciparum


ggplot(Fitness_MODEL_CV_OPT ,
       aes(x =B_V, y = endtime + 0.05, 
           fill =status))+
  geom_density(stat='identity',outline.type = 'full')+
  facet_wrap(~species)+
  scale_fill_manual(values = c('grey',"black",'#F14889'))+
  xlab("R")+
  ylab("Duration of acute phase")+
  theme_classic()+
  theme(strip.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text = element_text (size = 15))


ggplot(subset(Fitness_MODEL_CV_OPT ,
              Fitness_MODEL_CV_OPT$status == 'success'),
       aes(x = B_V, y = endtime + 0.05, 
           fill =species))+
  geom_density(stat='identity',outline.type = 'full', alpha = 0.5)+
  scale_fill_manual(name = "",values = c('#4B878BFF','#D01C1FFF'))+
  xlab("Burst size")+
  ylab("Duration of acute phase")+
  theme_classic()+
  theme(strip.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text = element_text (size = 15))



