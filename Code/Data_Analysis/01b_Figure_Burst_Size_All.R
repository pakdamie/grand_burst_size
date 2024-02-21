ggplot(Ordered_Ave_Species, 
         aes(x = Average, 
             y = Plasmodium.species, 
             fill = Subgenus)) +
  geom_vline(xintercept = c(10,20,30), linetype = 3)+
  geom_errorbar(
    aes(xmin = Lower, 
        xmax= Upper, 
        color = Subgenus))+
  geom_point(shape = 21, 
             size = 3) +
  xlab("Burst size")+
  ylab("")+
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks=seq(0,40,5))+
  scale_fill_manual(values = c("Bennettinia" = "#FFB713",
                               "Giovannolaia" = "#071952",
                               "Haemamoeba" = "#A5B5DF",
                               "Huffia" = "#088395",
                               "Novyella" = "#35A29F",
                               "Papernaia " = "#4F709C",
                               "vaughani" = "#E5D283",
                               "NA" = 'grey'))+
  scale_color_manual(values = c("Bennettinia" = "#FFB713",
                               "Giovannolaia" = "#071952",
                               "Haemamoeba" = "#A5B5DF",
                               "Huffia" = "#088395",
                               "Novyella" = "#35A29F",
                               "Papernaia " = "#4F709C",
                               "vaughani" = "#E5D283",
                               "NA" = 'grey'))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14,
                                   color = 'black'),
    axis.text.y = element_text(size = 12, 
                                 face = 'italic',
                                 color = 'black'),
        axis.title = element_text(size = 15))

ggsave(here("Figures", 
            "Raw", "01_Avian_burstsize_host_order_RAW.pdf"),
       width = 8, height = 12, units = 'in')


###AVIAN PLOT###
mal_mammal_GG <-
  ggplot(Ordered_Mam_Species, 
         aes(x = Average, 
             y = Plasmodium.species, 
             fill = Subgenus)) +
  geom_vline(xintercept = c(10,20,30), linetype = 3)+
  geom_errorbar(
    aes(xmin = Lower, 
        xmax= Upper, 
        color = Subgenus))+
  geom_point(shape = 21, 
             size = 3) +
  xlab("Burst size")+
  ylab("")+
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks=seq(0,40,5))+
  scale_fill_manual(values = c(" " = "grey",
                               "Laverania" = "#a86462",
                               "Plasmodium" = "#bd302c",
                               "Vinckeia" = "#560D0D"))+
  scale_color_manual(values = c(" " = "grey",
                                "Laverania" = "#a86462",
                                "Plasmodium" = "#bd302c",
                                "Vinckeia" = "#560D0D"))+
  theme_classic()+
  theme(axis.text = element_text(size = 12, 
                                 face = 'italic',
                                 color = 'black'),
        axis.title = element_text(size = 14))
mal_mammal_GG

ggsave(here("Figures", 
            "Raw",  "02_Mammal_burstsize_host_order.pdf"),
       width = 8, height = 12, units = 'in')




###Mammal and MAM_Tree - look at the species not found in the tree

mal_reptile_order_1 <- Ordered_Rep_Species[1:55,]
mal_reptile_order_2 <- Ordered_Rep_Species[56:111,]


###

mal_reptile_GG_1<-
  ggplot(mal_reptile_order_1, 
         aes(x = Average, 
             y = Plasmodium.species, 
             fill = Subgenus)) +
  geom_vline(xintercept = c(10,20,30,50,100), linetype = 3)+
  geom_errorbar(
    aes(xmin = Lower, 
        xmax= Upper, 
        color = Subgenus))+
  geom_point(shape = 21, 
             size = 3) +
  xlab("Burst size")+
  ylab("")+
  geom_point(shape = 21, 
             size = 3) + 
  xlab("Burst size") +
  ylab("Plasmodium species")+
  scale_x_continuous(breaks=seq(0,130,10))+
  scale_y_discrete(limits = rev) + 
  scale_fill_manual(values = c("Asiamoeba" = "grey",
                               "Carinamoeba" = "#a86462",
                               "Garnia" = "#bd302c",
                               "Lacertamoeba" = "#560D0D",
                               "Ophidiella",
                               "Paraplasmodium",
                               "Sauramoeba",
                               "Tropiduri",
                               "NA" = 'grey'))+
  scale_color_manual(values = c("Asiamoeba" = "grey",
                               "Carinamoeba" = "#a86462",
                               "Garnia" = "#bd302c",
                               "Lacertamoeba" = "#560D0D",
                               "Laverania" = "#a86462",
                               "Ophidiella",
                               "Paraplasmodium",
                               "Sauramoeba",
                               "Tropiduri",
                               "NA" = 'grey'))+
                                theme_classic()
  theme_classic()+
  theme(
    axis.text = element_text(size = 11.5, 
                             face = 'italic'),
    axis.title = element_text(size = 14),
    strip.text = element_blank())

mal_reptile_GG_2<-
  ggplot(mal_reptile_order_2, 
         aes(x = Average, 
             y = Plasmodium.species, 
             fill = Subgenus)) +
  geom_vline(xintercept = c(10,20,30,50,100), linetype = 3)+
  geom_segment(data=mal_reptile_order_2,
               aes(x = Lower, 
                   xend = Upper, 
                   y = Plasmodium.species,
                   yend = Plasmodium.species))+
  geom_point(shape = 21, 
             size = 3) + 
  xlab("Burst size") +
  ylab("Plasmodium species")+
  scale_x_continuous(breaks=seq(0,130,10))+
  scale_y_discrete(limits = rev) + 
  scale_fill_manual(values = c(" " = "grey",
                               "Laverania" = "#a86462",
                               "Plasmodium" = "#bd302c",
                               "Vinckeia" = "#560D0D"))+
  scale_color_manual(values = c(" " = "grey",
                                "Laverania" = "#a86462",
                                "Plasmodium" = "#bd302c")
  theme_classic()+
  theme(
    axis.text = element_text(size = 11.5, 
                             face = 'italic'),
    axis.title = element_text(size = 14),
    strip.text = element_blank())


mal_reptile_GG_1+ mal_reptile_GG_2 + patchwork::plot_layout(guides = 'collect')

ggsave(here("Figure", "Data_Analysis", 
            "Raw", "03_Reptile_burstsize_host_order.pdf"),
       width = 20, height = 13, units = 'in')