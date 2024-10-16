### This script cleans the data to create the large burst size Figure 1

library(here)

# Load necessary functions and data
source(here("Code", "Functions", "FUNC_Order_Plasmodium_Host.R"))
source(here("Code", "Data_Analysis", "00_full_phylogeny_tree.R"))
mal_dat <- read.csv(here("Data", "MALARIA_PAK_SPECIES.csv"))

# Remove duplicate entries (different asexual development times found)
mal_dat[mal_dat$Plasmodium.species %in% c('circumflexum_2', 'lophurae_2'), ] <- NA

# Subset data for avian, mammal, and reptile hosts
mal_avian <- subset(mal_dat, mal_dat$OrderHost == 'avian' & mal_dat$Include != "No")
mal_mammal <- subset(mal_dat, mal_dat$OrderHost == 'mammal' & mal_dat$Include != "No")
mal_reptile <- subset(mal_dat, mal_dat$OrderHost == 'reptile' & mal_dat$Include != "No")

### Avian hosts: Process and order species for the tree
mal_avian$Type.Host <- sub(" ", "_", mal_avian$Type.Host)

# Extract tip labels and order them
tip_name_ave <- ggtree(keep.tip(consensus_AVE_Tree, mal_avian$Type.Host))[["data"]][, c('label', 'y')]
tip_name_ave <- tip_name_ave[order(tip_name_ave$y), ]
tip_name_ave <- subset(tip_name_ave, (tip_name_ave$y) %% 1 == 0 & tip_name_ave$label != 1)

# Order Plasmodium species based on burst size
Ordered_Ave_Species <- Ordering_PlasmodiumSP_Burst(mal_avian, tip_name_ave)
Ordered_Ave_Species$Plasmodium.species <- factor(Ordered_Ave_Species$Plasmodium.species,
                                                 levels = Ordered_Ave_Species$Plasmodium.species)

### Mammal hosts: Process and order species for the tree
mal_mammal$Type.Host <- sub(" ", "_", mal_mammal$Type.Host)

# Extract tip labels and order them
tip_name_mam <- ggtree(keep.tip(consensus_MAM_Tree, mal_mammal$Type.Host))[["data"]][, c('label', 'y')]
tip_name_mam <- tip_name_mam[order(tip_name_mam$y), ]
tip_name_mam <- subset(tip_name_mam, tip_name_mam$y %% 1 == 0 & tip_name_mam$label != 1)

# Order Plasmodium species based on burst size
Ordered_Mam_Species <- Ordering_PlasmodiumSP_Burst(mal_mammal, tip_name_mam)
Ordered_Mam_Species$Plasmodium.species <- factor(Ordered_Mam_Species$Plasmodium.species,
                                                 levels = Ordered_Mam_Species$Plasmodium.species)

### Reptile hosts: Process and order species for the tree
mal_reptile$Type.Host <- sub(" ", "_", mal_reptile$Type.Host)

# Extract tip labels and order them
tip_name_rep <- ggtree(keep.tip(consensus_REP_Tree, mal_reptile$Type.Host))[["data"]][, c('label', 'y')]
tip_name_rep <- tip_name_rep[order(tip_name_rep$y), ]
tip_name_rep <- subset(tip_name_rep, tip_name_rep$y %% 1 == 0 & tip_name_rep$label != 1)

# Order Plasmodium species based on burst size
Ordered_rep_Species <- Ordering_PlasmodiumSP_Burst(mal_reptile, tip_name_rep)
Ordered_rep_Species$Plasmodium.species <- factor(Ordered_rep_Species$Plasmodium.species,
                                                 levels = Ordered_rep_Species$Plasmodium.species)

# Split reptile species into two groups
mal_reptile_order_1 <- Ordered_rep_Species[1:51, ]
mal_reptile_order_2 <- Ordered_rep_Species[52:103, ]

### Proceed to 01b_Figure_Burst_Size_All.R