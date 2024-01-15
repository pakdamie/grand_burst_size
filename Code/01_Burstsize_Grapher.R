library(here)

###making the large figure of species and average/low/upper
###burst size

mal_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

###circumflexum_1 get rid of (which is row 50)

mal_dat[mal_dat$Plasmodium.species=='circumflexum_2',] <- NA
###Groupped_Data
mal_avian <- subset(mal_dat, mal_dat$OrderHost == 'avian' &
                      mal_dat$Include != "No")
mal_mammal<- subset(mal_dat, mal_dat$OrderHost == 'mammal')
mal_reptile <- subset(mal_dat, mal_dat$OrderHost == 'reptile')
###Reordering

###Avian###
avian_species_order <- mal_avian$Plasmodium.species[order(mal_avian$Order)]
mal_avian$Plasmodium.species <- factor(mal_avian$Plasmodium.species,
                                       levels = avian_species_order)
mal_avian <- subset(mal_avian, mal_avian$Include == "")
###Mammal###
mam_species_order <- mal_mammal$Plasmodium.species[order(mal_mammal$Order)]
mal_mammal$Plasmodium.species <- factor(mal_mammal$Plasmodium.species,
                                       levels = mam_species_order)



###AVIAN PLOT###
mal_avian_GG <-
  ggplot(mal_avian, 
        aes(x = Average, 
            y = Plasmodium.species, 
            fill = Subgenus)) +
   geom_segment(data=mal_avian,
         aes(x = Lower, 
             xend = Upper, 
             y = Plasmodium.species,
             yend = Plasmodium.species))+
   geom_point(shape = 21, 
              size = 3
              ) + 
   xlab("Burst size")+
   ylab("Plasmodium species")+
   scale_x_continuous(breaks=seq(0,40,5))+
   scale_y_discrete(limits = rev) + 
   scale_color_viridis()+
   theme_classic()+
   theme(axis.text = element_text(size = 11.5, 
                                  face = 'italic'),
         axis.title = element_text(size = 14))
mal_avian_GG
ggsave(here("Figure", "Data_Analysis", 
            "Raw", "01_Avian_burstsize_host_order_Final.pdf"),
       width = 8, height = 10, units = 'in')
 
###MAMMAL PLOT###
mal_mammal_GG <-
  ggplot(mal_mammal, 
         aes(x = Average, 
             y = Plasmodium.species, 
             fill=Subgenus)) +
  geom_segment(data=mal_mammal,
               aes(x = Lower, 
                   xend = Upper, 
                   y = Plasmodium.species,
                   yend = Plasmodium.species,color = 
                     Subgenus))+
  geom_point(shape = 21, 
             size = 3) + 
  xlab("Burst size")+
  ylab("Plasmodium species")+
  scale_x_continuous(breaks=seq(0,40,5))+
  scale_y_discrete(limits = rev) + 
  scale_fill_viridis(discrete = TRUE)+
  scale_color_viridis(discrete = TRUE)+
  
  theme_classic()+
  theme(
        axis.text = element_text(size = 11.5, 
                                 face = 'italic'),
        axis.title = element_text(size = 14))

ggsave(here("Figure", "Data_Analysis", 
            "Raw", "02_Mammal_burstsize_host_order.pdf"),
       width = 8, height = 11, units = 'in')

###Reptile###
###Too long so going to split into two graphs (cut in half)

mal_reptile_order <- mal_reptile[order(mal_reptile$Order),]
mal_reptile_order_1 <- mal_reptile_order[1:70,]
mal_reptile_order_2 <- mal_reptile_order[71:113,]
###Reptile###
reptile_species_order_1 <- mal_reptile_order_1$Plasmodium.species[order(mal_reptile_order_1$Order)]
mal_reptile_order_1$Plasmodium.species <- factor(mal_reptile_order_1$Plasmodium.species,
                                        levels = reptile_species_order_1)


reptile_species_order_2 <- mal_reptile_order_2$Plasmodium.species[order(mal_reptile_order_2$Order)]
mal_reptile_order_2$Plasmodium.species <- factor(mal_reptile_order_2$Plasmodium.species,
                                                 levels = reptile_species_order_2)

###Reptile PLOT###
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


mal_reptile_GG_1+ mal_reptile_GG_2

ggsave(here("Figure", "Data_Analysis", 
            "Raw", "03_Reptile_burstsize_host_order.pdf"),
       width = 20, height = 13, units = 'in')
