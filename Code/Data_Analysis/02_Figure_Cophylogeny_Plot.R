#This script is to make PANEL B of the cophylogeny plot (Figure 3)
#
# Select specific columns from the mal_dat_asex data frame and create an association matrix
association_matrix <- 
  mal_dat_asex[, c("Plasmodium_species", "Host", "Upper_Burst_Size")]

# Calculate the maximum burst size for each Plasmodium species
# regardless of host species
maximum_by_parasite <- tapply(
  association_matrix$Upper_Burst_Size, 
  association_matrix$Plasmodium_species, 
  max
  )

maximum_by_parasite_DF <- data.frame(
  bs = maximum_by_parasite, 
  species = names(maximum_by_parasite)
  )

# Calculate the maximum burst size for each Host species regardless of 
# Plasmodium species
maximum_by_host <- tapply(
        association_matrix$Upper_Burst_Size, 
        association_matrix$Host, 
        max
)

maximum_by_host_DF <- data.frame(
  bs = maximum_by_host, 
  species = names(maximum_by_host)
)

# Here, I prune down the host trees to only include the hosts associated with the 
# parasite data that we have the phylogenetic information for
type_host_tree_subsetted <- drop.tip(
  Full_SuperTree_Host,
  Full_SuperTree_Host$tip.label[
    -match(mal_dat_asex$Host, Full_SuperTree_Host$tip.label)
  ]
)

#This creates the cophylogeny data.frame that is used for the model
cophylogeny_data_frame <- cophylo(
  plasmodium_tree_full,
  type_host_tree_subsetted,
  association_matrix) 

parasite_order_tree <- data.frame(
  species = rev(cophylogeny_data_frame$trees[[1]]$tip.label)
)
# Assign position values sequentially from 1 to the number of species
parasite_order_tree$position <- seq(1, nrow(parasite_order_tree))

# Merge the parasite order tree with the maximum burst size data for parasites
parasite_order_tree <- left_join(parasite_order_tree, maximum_by_parasite_DF)

# Convert the species column to a factor with levels in reverse order
parasite_order_tree$species <- factor(
  parasite_order_tree$species, 
  levels = rev(parasite_order_tree$species)
)

# Create the host order tree
host_order_tree <- data.frame(
  species = rev(cophylogeny_data_frame$trees[[2]]$tip.label)
)

# Assign position values sequentially from 1 to the number of species
host_order_tree$position <- seq(1, nrow(host_order_tree))

# Merge the host order tree with the maximum burst size data for parasites
host_order_tree <- left_join(host_order_tree, maximum_by_host_DF)

# Convert the species column to a factor with levels in reverse order
host_order_tree$species <- factor(
  host_order_tree$species, 
  levels = rev(host_order_tree$species)
)

#I have to do a hackey - way to put the color points

###PARASITES
par_order_points <- ggplot(parasite_order_tree, 
  aes(x = 1, y = species, fill = log10(bs))) +
  geom_point(size = 3, shape = 21) + 
  scale_fill_viridis(option = 'inferno') +
  xlab("") +
  ylab("") +
  theme_classic() + 
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
    )

ggsave(par_order_points, 
  file = here("Figures", "Raw", "par_order_points.pdf"),
  units = 'in', width = 3, height = 12
  )

###HOSTS
host_order_points <- ggplot(host_order_tree, 
  aes(x = 1, y = species, fill = log10(bs))) +
  geom_point(size = 3, shape = 21) + 
  scale_fill_viridis(option = 'inferno') +
  theme_classic() + 
  xlab("")+
  ylab("")+
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

ggsave(host_order_points, 
  file = here("Figures", "Raw", "host_order_points.pdf"),
  units = 'in', width = 3, height = 12
  )

# THIS REQUIRES A LOT OF MANUAL EDITING IN ILLUSTRATOR
# For Questions of Reproducibility: The plot(cophylo(...)) 
# is largely unchanged (I just visually tweaked the trees
# to be a bit more visible! But the raw format created by
# this script IS THE EXACT SAME ORDER)

# The color of the maximum burst size for the host AND parasite
# are in order as the phylogenetic trees and you can see it separately!
# I put these together in Illustrator due to the weirdness of the 
# cophylo points. 


#THE RAW PANEL A OF FIGURE 3.
pdf(file = here("Figures", "Raw", "cophylo_Raw_plot.pdf"),   
  width = 14, 
  height = 10)

plot(cophylogeny_data_frame,
     rotate = TRUE)

dev.off()

#END