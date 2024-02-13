###This script is to clean the data to make the large
###burst size figure 
library(here)

mal_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

###We need to get rid of circumflexum_1 get rid of (which is row 50) 
###as it is a duplicate entry (two different asexual development time
###was found)
mal_dat[mal_dat$Plasmodium.species %in%  c('circumflexum_2','lophurae_2'),] <- NA

###No longer manually doing it with my own ordering, 
###but with the phylo tree

#Include = No is important

mal_avian <- subset(mal_dat, mal_dat$OrderHost == 'avian' &
                      mal_dat$Include != "No")
mal_mammal<- subset(mal_dat, mal_dat$OrderHost == 'mammal' &
                      mal_dat$Include != "No")
mal_reptile <- subset(mal_dat, mal_dat$OrderHost == 'reptile' &
                      mal_dat$Include != "No")

###Avian and consensus_AVE_Tree - look at the species not found in the tree
mal_avian$Type.Host <- sub(" ", "_", mal_avian$Type.Host)

subset(mal_avian, !(mal_avian$Type.Host %in% consensus_AVE_Tree$tip.label))$Type.Host

tip_name_ave <- ggtree(consensus_AVE_Tree)[["data"]][,c('label','y')]
tip_name_ave <- tip_name_ave [order(tip_name_ave$y),]
tip_name_ave <- subset(tip_name_ave, tip_name_ave$label != '1')

Ordered_Ave_Species <- Ordering_PlasmodiumSP_Burst(mal_avian,tip_name_ave )

Ordered_Ave_Species$Plasmodium.species <- factor(Ordered_Ave_Species $Plasmodium.species,
                                                  levels = Ordered_Ave_Species$Plasmodium.species )


###Mammal and MAM_Tree - look at the species not found in the tree
mal_mammal$Type.Host <- sub(" ", "_", mal_mammal$Type.Host)

subset(mal_mammal, !(mal_mammal$Type.Host %in% consensus_MAM_Tree$tip.label))$Type.Host

tip_name_mam <- ggtree(consensus_MAM_Tree)[["data"]][,c('label','y')]
tip_name_mam <- tip_name_mam[order(tip_name_mam$y),]
tip_name_mam <- subset(tip_name_mam, tip_name_mam$label != '1')

Ordered_Mam_Species <- Ordering_PlasmodiumSP_Burst(mal_mammal,tip_name_mam )

Ordered_Mam_Species$Plasmodium.species <- factor(Ordered_Mam_Species $Plasmodium.species,
                                                 levels = Ordered_Mam_Species$Plasmodium.species )



###AVIAN PLOT###
mal_avian_GG <-
  ggplot(Ordered_Ave_Species, 
        aes(x = Average, 
            y = Plasmodium.species, 
            fill = Subgenus)) +
   geom_segment(
         aes(x = Lower, 
             xend = Upper, 
             y = Plasmodium.species,
             yend = Plasmodium.species))+
   geom_point(shape = 21, 
              size = 3
              ) +
   xlab("Burst size")+
   ylab("")+
   scale_x_continuous(breaks=seq(0,40,5))+
   scale_fill_manual(values = c("Bennettinia" = "#F2F7A1",
                                "Giovannolaia" = "#071952",
                                "Huffia" = "#088395",
                                "Novyella" = "#35A29F",
                                "Papernaia " = "#4F709C",
                                "vaughani" = "#E5D283",
                                "NA" = 'grey'))+
   theme_classic()+
   theme(axis.text = element_text(size = 12, 
                                  face = 'italic',
                                  color = 'black'),
         axis.title = element_text(size = 14))
mal_avian_GG

ggsave(here("Figures", 
            "Raw", "01_Avian_burstsize_host_order_Final.pdf"),
             width = 8, height = 12, units = 'in')
 

###AVIAN PLOT###
mal_mammal_GG <-
  ggplot(Ordered_Mam_Species, 
         aes(x = Average, 
             y = Plasmodium.species, 
             fill = Subgenus)) +
  geom_segment(
    aes(x = Lower, 
        xend = Upper, 
        y = Plasmodium.species,
        yend = Plasmodium.species))+
  geom_point(shape = 21, 
             size = 3
  ) +
  xlab("Burst size")+
  ylab("")+
  scale_x_continuous(breaks=seq(0,40,5))+
  scale_fill_manual(values = c(" " = "grey",
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




###Avian###
avian_species_order <- mal_avian$Plasmodium.species[order(mal_avian$Order)]
mal_avian$Plasmodium.species <- factor(mal_avian$Plasmodium.species,
                                       
                                       levels = avian_species_order)
mal_avian <- subset(mal_avian, mal_avian$Include == "")
###Mammal###
mam_species_order <- mal_mammal$Plasmodium.species[order(mal_mammal$Order)]
mal_mammal$Plasmodium.species <- factor(mal_mammal$Plasmodium.species,
                                        levels = mam_species_order)



###Mammal and MAM_Tree - look at the species not found in the tree
mal_reptile$Type.Host <- sub(" ", "_", mal_reptile$Type.Host)

subset(mal_reptile, !(mal_reptile$Type.Host %in% consensus_REP_Tree$tip.label))$Type.Host

tip_name_rep <- ggtree(consensus_REP_Tree)[["data"]][,c('label','y')]
tip_name_rep <- tip_name_rep[order(tip_name_rep$y),]
tip_name_rep <- subset(tip_name_rep, tip_name_rep$label != '1')

Ordered_Rep_Species <- Ordering_PlasmodiumSP_Burst(mal_reptile,tip_name_rep )

Ordered_Rep_Species$Plasmodium.species <- factor(Ordered_Rep_Species $Plasmodium.species,
                                                 levels = Ordered_Rep_Species$Plasmodium.species )

mal_reptile_order_1 <- Ordered_Rep_Species[1:55,]
mal_reptile_order_2 <- Ordered_Rep_Species[56:111,]


###

mal_reptile_GG_1<-
  ggplot(mal_reptile_order_1, 
         aes(x = Average, 
             y = Plasmodium.species, 
             fill = Subgenus)) +
  geom_segment(data=mal_reptile_order_1,
               aes(x = Lower, 
                   xend = Upper, 
                   y = Plasmodium.species,
                   yend = Plasmodium.species))+
  geom_point(shape = 21, 
             size = 3) + 
  xlab("Burst size") +
  ylab("Plasmodium species")+
  scale_x_continuous(breaks=seq(0,300,10))+
  scale_y_discrete(limits = rev) + 
  scale_fill_viridis(discrete = TRUE)+
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
  geom_segment(data=mal_reptile_order_2,
               aes(x = Lower, 
                   xend = Upper, 
                   y = Plasmodium.species,
                   yend = Plasmodium.species))+
  geom_point(shape = 21, 
             size = 3) + 
  xlab("Burst size") +
  ylab("Plasmodium species")+
  scale_x_continuous(breaks=seq(0,300,10))+
  scale_y_discrete(limits = rev) + 
  scale_fill_viridis(discrete = TRUE)+
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
