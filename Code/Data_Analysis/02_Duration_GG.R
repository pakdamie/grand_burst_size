###Figure making script that looks at the 
###asexual development time and the maximum burst size to see if
###there is any relationship between them.
###This is the plot making figure

library(here)
source(here("Code","Functions", "FUNC_Package_Loader.R"))
source(here("Code", "Data_Analysis","00_parasite_phylogeny_tree.R"))

###Malaria Data - Species
mal_dat_asex <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

###Ensures that we're not looking at malaria species that were excluded
mal_dat_asex <- subset(mal_dat_asex, mal_dat_asex$Include != "No")

mal_dat_asex_Subsetted <- subset(mal_dat_asex[,c("Upper","Duration")])
weird_table <- data.frame(table(mal_dat_asex_Subsetted))


ggplot(weird_table, aes(x = Duration, y= Upper, size = Freq, color = as.factor(Freq)))+
  geom_point()+
  scale_color_manual(values = c('white','black', 
                                'black',
                                'black',
                                'black', 
                                'black',
                                'black'))+
  theme_classic()

UPPER_GG  <- ggplot(mal_dat_asex, 
                    aes(x = (Duration), 
                        y = Upper, 
                        fill = Duration, 
                        group = Duration))+
  geom_violin(alpha = 0.2, color = NA)+
  geom_beeswarm(size = 2.5, shape = 21, cex = 1.2, color = "NA")+
  scale_fill_viridis(option = 'turbo')+
  scale_x_continuous(breaks = c(0, 24,36,48,60, 72,84, 96))+
  scale_y_continuous(limits = c(0,40), 
                     breaks = seq(0,40,5))+
  xlab("Duration (Hours)") +
  ylab("Upper burst size") +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))
