
### This is the full figure for making the cophylogeny plot 
### between host and parasites (Figure 2) 

library(here)
source(here("Code","Functions","FUNC_full_data_loader.R"))

###Create an association-matrix for the cophylogeny plot
association_matrix <- mal_dat_asex[, c("Plasmodium_species", "Host", "Upper_Burst_Size")]

maximum_by_parasite <- do.call(rbind,(by(association_matrix,association_matrix$Plasmodium_species, 
                       function(x) max(x$Upper_Burst_Size),
                       simplify = FALSE)))

maximum_by_parasite_DF <- cbind.data.frame(bs = maximum_by_parasite,species = row.names(maximum_by_parasite))

maximum_by_host <- do.call(rbind,(by(association_matrix,association_matrix$Host, 
                                 function(x) max(x$Upper_Burst_Size),
                                 simplify = FALSE)))
maximum_by_host_DF <- cbind.data.frame(bs = maximum_by_host, species = row.names(maximum_by_host))



### Pruning down the host trees to only include the hosts associated with the 
### parasite data that we have the phylogenetic tree

type_host_tree_subsetted <- drop.tip(
  Full_SuperTree_Host,
  Full_SuperTree_Host$tip.label
  [-match(mal_dat_asex$Host, Full_SuperTree_Host$tip.label)]
)

cophylogeny_data_frame <- cophylo(
               plasmodium_tree_full,
              type_host_tree_subsetted,
              association_matrix) 


parasite_order_tree <- data.frame(species = rev(cophylogeny_data_frame$trees[[1]]$tip.label))
parasite_order_tree$position <- seq(1, nrow(parasite_order_tree))
parasite_order_tree <- left_join(parasite_order_tree, maximum_by_parasite_DF)
parasite_order_tree$species <- factor(parasite_order_tree$species, levels = rev(parasite_order_tree$species))



host_order_tree <- data.frame(species = rev(cophylogeny_data_frame$trees[[2]]$tip.label))
host_order_tree$position <- seq(1, nrow(host_order_tree))
host_order_tree <- left_join(host_order_tree, maximum_by_host_DF)
host_order_tree$species <- factor(host_order_tree$species, levels = rev(host_order_tree$species))



par_order_points <- ggplot(parasite_order_tree, aes(x = 1, y = species, fill = log10(bs))) +
  geom_point(size = 3, shape = 21) + scale_fill_viridis(option = 'inferno') +
  theme_classic() + 
  xlab("")+
  ylab("")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(par_order_points, file = here("Figures", "Raw", "par_order_points.pdf"),
       units = 'in', width = 3, height = 12)



host_order_points <-ggplot(host_order_tree, aes(x = 1, y = species, fill = log10(bs))) +
  geom_point(size = 3, shape = 21) + scale_fill_viridis(option = 'inferno') +
  theme_classic() + 
  xlab("")+
  ylab("")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(host_order_points, file = here("Figures", "Raw", "host_order_points.pdf"),
       units = 'in', width = 3, height = 12)

### THIS REQUIRES A LOT OF MANUAL EDITING IN ILLUSTRATOR
### For Questions of Reproducibility: The plot(cophylo(...)) 
### is largely unchanged (I just visually tweaked, the trees
### to be a bit more visible)

### The color of the maximum burst size for the host AND parasite
### are in order as the phylogenetic trees and you can see it separately!
### I put these togehter in Illustrator! 



pdf(file = here("Figures", "Raw", "cophylo_Raw_plot.pdf"),   # The directory you want to save the file in
    width = 14, # The width of the plot in inches
    height = 10)

plot(cophylogeny_data_frame,
     rotate = TRUE)
dev.off()

