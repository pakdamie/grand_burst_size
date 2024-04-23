library(here)
source(here("Code", "Functions", "FUNC_Package_Loader.R"))
source(here("Code", "Data_Analysis", "00_parasite_phylogeny_tree.R"))


### Use the subsetted one
mal_dat_asex <- read.csv(here("Data", "MALARIA_PHYLOGENY_HOST_VARIOUS.csv"))
mal_dat_asex$Host <- add_underscore(mal_dat_asex$Host)


association_matrix <- mal_dat_asex[, c("Plasmodium_species", "Host", "Upper_Burst_Size")]


### Pruning down the host trees for the analysis as well as making the cophylogenetic
### figure

type_host_tree_subsetted <- drop.tip(
  Full_SuperTree_Host,
  Full_SuperTree_Host$tip.label
  [-match(mal_dat_asex$Host, Full_SuperTree_Host$tip.label)]
)

plot(cophylo(
  plasmodium_tree_full,
  type_host_tree_subsetted,
  association_matrix
), rotate = TRUE, use.edge.length = FALSE)

### THIS REQUIRES A LOT OF MANUAL EDITING IN ILLUSTRATOR

### OK let's create a color vector
ggplot(association_matrix, aes(
  x = Plasmodium_species,
  y = Host,
  fill = log(Upper_Burst_Size)
)) +
  geom_tile(size = 3) +
  scale_fill_viridis(option = "inferno") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
