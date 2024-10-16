# This is a simple figure to create the mouse - human plots

source(here("Code", "Functions", "FUNC_DA_full_data_loader.R"))

#This is the data.frame that contains only the rodent
Muridae_DF <- subset(mal_dat_asex_full,
  mal_dat_asex_full$Plasmodium.species %in% 
  c("aegyptensis","berghei", "chabaudi","vinckei"))

#Highlight only berghei and chabaudi 
Muridae_DF$highlight <- "No"

Muridae_DF[Muridae_DF$Plasmodium.species %in% 
  c("berghei", "chabaudi"),]$highlight = "Yes" 


#Make the ggplot
Mouse_Burst_GG <- ggplot(Muridae_DF, 
  aes(x = Upper, y = Plasmodium.species, color = highlight)) + 
  geom_point(size = 3) +
  geom_segment(aes(x = Lower, xend = Upper, y = Plasmodium.species,
    yend = Plasmodium.species), size =0.8) +
  ylab("") +
  xlab("Burst size") +
  xlim(0,40) +
  scale_color_manual(values = c("Yes" = "black", "No" = "darkgrey")) +
  scale_y_discrete(limits=rev) +
  theme_classic() +
  theme(legend.position = "none",
    axis.text = element_text(color = 'black', size = 14),
    axis.title = element_text(color = 'black', size = 15))


### Human figure 

Human_DF <- subset(mal_dat_asex_full,
  mal_dat_asex_full$Family %in% c("Human")|
  mal_dat_asex_full$Plasmodium.species == "falciparum" )

Human_DF$highlight <- "No"

Human_DF[Human_DF$Plasmodium.species %in% c("falciparum", "vivax"),]$highlight = "Yes" 


Human_Burst_GG <- ggplot(Human_DF, 
  aes(x = Upper, y = Plasmodium.species, color = highlight)) + 
  geom_point(size=3) +
  geom_segment(aes(x = Lower, xend = Upper, y = Plasmodium.species,
  yend = Plasmodium.species),size = 0.8) +
  ylab("") +
  xlab("Burst size") +
  xlim(0,40) +
  scale_color_manual(values = c("Yes" = "black", "No" = "darkgrey"))+
  scale_y_discrete(limits=rev)+
  theme_classic()+
  theme(legend.position = "none",
  axis.text = element_text(color = 'black', size = 14),
  axis.title = element_text(color = 'black', size = 15))

Mouse_Burst_GG  + Human_Burst_GG



ggsave(here("Figures", "Raw", "human_mice_burstsize.pdf"),
       width = 10, height = 3.5, units = "in")
