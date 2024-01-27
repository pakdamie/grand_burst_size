###Plot the phylogenetic trees of the host and the upper burst size
###Seperate into different orders because the entire full plot is too 
###hard to see
library(here)
source(here("Code", "Functions", "util_remove_underscore.R"))
source(here("Code","05_phylogeny_full_combiner.R"))

###FULL TREE

ggtree(FULL_Merged_Phylogeny, layout="fan", open.angle=10) + 
  geom_tippoint(mapping=aes(fill=(Upper)), 
                size=5, shape = 21)+
  scale_fill_viridis(option='turbo')


ggsave(here("Figures","Raw", "FULL_Phylo_Upper_Burst.pdf"), width = 20,
       height = 20,units = 'in')


###Reptiles
REP_DATA_ASEXUAL_GG <- gheatmap(ggtree(Reptile_Data_Merged_Phylogeny),
                                data = tdata(Reptile_Data_Merged_Phylogeny),
                                low = "#0a50a1",
                                high = "#e1000c",
                                color = "black",
                                width = 0.3,
                                legend_title  = "Upper \n burst size") +  
  geom_tiplab(offset = 10)+
  hexpand(0.5) + vexpand(.05, -1) + 
  theme(legend.position = c(.1, .75))

ggsave(here("Figures","Raw", "Reptile_Phylo_Upper_Burst.pdf"), width = 5,
       height = 14,units = 'in')

###Birds
AVE_DATA_ASEXUAL_GG <- gheatmap(ggtree(Avian_Data_Merged_Phylogeny),
                                data = tdata(Avian_Data_Merged_Phylogeny),
                                low = "#0a50a1",
                                high = "#e1000c",
                                color = "black",
                                width = 0.3,
                                legend_title  = "Upper \n burst size") +  
  geom_tiplab(offset = 10)+
  hexpand(0.5) + vexpand(.05, -1) + 
  theme(legend.position = c(.1, .75))

ggsave(here("Figures","Raw", "Avian_Phylo_Upper_Burst.pdf"), width = 5,
       height = 14,units = 'in')

###Mammal
MAM_DATA_ASEXUAL_GG <- gheatmap(ggtree(Mammal_Data_Merged_Phylogeny),
                                data = tdata(Mammal_Data_Merged_Phylogeny),
                                low = "#0a50a1",
                                high = "#e1000c",
                                color = "black",
                                width = 0.3,
                                legend_title  = "Upper \n burst size") +  
  geom_tiplab(offset = 40)+
  hexpand(0.5) + vexpand(.05, -1) + 
  theme(legend.position = c(.1, .75))

ggsave(here("Figures","Raw", "Mammal_Phylo_Upper_Burst.pdf"), width = 5,
       height = 14,units = 'in')
