### This script is to plot the facet and show that there is
### a lot of confusion when it comes to talking about RBC tropism.
### Run this script when you want to get the facet for Figure 3.

library(here)
source(here("Code", "Functions", "FUNC_full_data_loader.R"))

### Subsetted from the main_dat
subsetted_preference_dat <- mal_dat_asex_full[, c(
  "Plasmodium.species", "Type_Host",
  "Known_Preference", "Inferred_Preference",
  "Higher.burst.size.in", "Infectivity_note"
)]

### Get rid of circumflexum and lophurae AND
### Get only the data where there is some information about the preference
subsetted_preference_dat <- subset(
  subsetted_preference_dat,
  is.na(subsetted_preference_dat$Known_Preference) == FALSE &
    !(subsetted_preference_dat$Plasmodium.species %in% c("lophurae_2", "circumflexum_2"))
)

### Prune the full host tree so that it only includes the type host that we are working above.
pruned.tree <- drop.tip(
  Full_SuperTree_Host,
  setdiff(
    Full_SuperTree_Host$tip.label,
    unique(subsetted_preference_dat$Type_Host)
  )
)

### This is to ensure the ordering
pruned.tree_host_order <- cbind.data.frame(order = seq(1, length(pruned.tree$tip.label)), Type_Host = pruned.tree$tip.label)
ordered_pruned_dat <- left_join(subsetted_preference_dat, pruned.tree_host_order)
ordered_pruned_dat$Plasmodium.species <- factor(ordered_pruned_dat$Plasmodium.species,
  levels = c(ordered_pruned_dat$Plasmodium.species[order(ordered_pruned_dat$order)])
)


ordered_pruned_dat <- ordered_pruned_dat[order(ordered_pruned_dat$order), ]
ordered_pruned_dat$facet_var <- NA
### Basically have it so that we split by facet.
ordered_pruned_dat$facet_var[ordered_pruned_dat$order %in% seq(1, 16)] <- 1
ordered_pruned_dat$facet_var[ordered_pruned_dat$order %in% seq(17, 33)] <- 2
ordered_pruned_dat$facet_var[ordered_pruned_dat$order %in% seq(34, 54)] <- 3
ordered_pruned_dat$facet_var[ordered_pruned_dat$order %in% seq(55, 89)] <- 4


### These are the species where there is a conflict
only_conflict_df <- subset(ordered_pruned_dat, ordered_pruned_dat$Known_Preference %in% c(
  "Conflict(All,Immature)*",
  "Conflict(All,Mature)*", "Conflict(All,Mature)",
  "Conflict(All;Mature)",
  "Conflict(Immature,Mature)*",
  "Conflict(Immature,Mature)"
) |
  ordered_pruned_dat$Inferred_Preference %in% c(
    "Conflict(All,Immature)*",
    "Conflict(All,Mature)*", "Conflict(All,Mature)",
    "Conflict(Immature,Mature)*",
    "Conflict(Immature,Mature)"
  ))


meltedconflict_df <- melt(only_conflict_df[, -6], id.vars = c("Plasmodium.species", "Type_Host", "order", "facet_var"))
meltedconflict_df$point1 <- "Square"
meltedconflict_df$point2 <- "Square"
meltedconflict_df$color1 <- NA
meltedconflict_df$color2 <- NA

meltedconflict_df[meltedconflict_df$value == "Conflict(All,Immature)*", ]$point1 <- "a"
meltedconflict_df[meltedconflict_df$value == "Conflict(All,Immature)*", ]$color1 <- "Immature"

meltedconflict_df[meltedconflict_df$value == "Conflict(All,Immature)*", ]$point2 <- "b"
meltedconflict_df[meltedconflict_df$value == "Conflict(All,Immature)*", ]$color2 <- "All"

meltedconflict_df[meltedconflict_df$value %in% c("Conflict(All,Mature)*", "Conflict(All,Mature)", "Conflict(All;Mature)"), ]$point1 <- "a"
meltedconflict_df[meltedconflict_df$value %in% c("Conflict(All,Mature)*", "Conflict(All,Mature)", "Conflict(All;Mature)"), ]$color1 <- "Mature"

meltedconflict_df[meltedconflict_df$value %in% c("Conflict(All,Mature)*", "Conflict(All,Mature)", "Conflict(All;Mature)"), ]$point2 <- "b"
meltedconflict_df[meltedconflict_df$value %in% c("Conflict(All,Mature)*", "Conflict(All,Mature)", "Conflict(All;Mature)"), ]$color2 <- "All"

meltedconflict_df[meltedconflict_df$value %in% c("Conflict(Immature,Mature)*", "Conflict(Immature,Mature)"), ]$point1 <- "a"
meltedconflict_df[meltedconflict_df$value %in% c("Conflict(Immature,Mature)*", "Conflict(Immature,Mature)"), ]$color1 <- "Immature"

meltedconflict_df[meltedconflict_df$value %in% c("Conflict(Immature,Mature)*", "Conflict(Immature,Mature)"), ]$point2 <- "b"
meltedconflict_df[meltedconflict_df$value %in% c("Conflict(Immature,Mature)*", "Conflict(Immature,Mature)"), ]$color2 <- "Mature"

meltedconflict_df[meltedconflict_df$value %in% c("Immature"), ]$color1 <- "Immature"
meltedconflict_df[meltedconflict_df$value %in% c("Mature"), ]$color1 <- "Mature"
meltedconflict_df[meltedconflict_df$value %in% c("Unknown"), ]$color1 <- "Unknown"
meltedconflict_df[meltedconflict_df$value %in% c("All"), ]$color1 <- "All"

### No conflict

### These are the species where there is no conflict
non_conflict_df <- subset(ordered_pruned_dat, !(ordered_pruned_dat$Plasmodium.species %in% only_conflict_df$Plasmodium.species))


meltednonconflict_df <- melt(non_conflict_df[, -6], id.vars = c("Plasmodium.species", "Type_Host", "order", "facet_var"))
meltednonconflict_df$point1 <- "Square"
meltednonconflict_df$point2 <- "Square"
meltednonconflict_df$color1 <- NA
meltednonconflict_df$color2 <- NA

meltednonconflict_df[meltednonconflict_df$value %in% c("Immature", "Immature*"), ]$color1 <- "Immature"
meltednonconflict_df[meltednonconflict_df$value %in% c("Mature", "Mature*"), ]$color1 <- "Mature"
meltednonconflict_df[meltednonconflict_df$value %in% c("All", "All*"), ]$color1 <- "All"
meltednonconflict_df[meltednonconflict_df$value %in% c("Unknown", NA), ]$color1 <- "Unknown"
meltednonconflict_df[meltednonconflict_df$value %in% c("No"), ]$color1 <- "No"


squared_melted_conflict <- subset(meltedconflict_df, meltedconflict_df$point1 == "Square")
full_melted_df <- rbind.data.frame(squared_melted_conflict, meltednonconflict_df)
full_melted_df[is.na(full_melted_df$color1) == TRUE, ]$color1 <- "Unknown"


ggplot(full_melted_df, aes(x = Plasmodium.species))+
  
  theme(axis.text.x = element_text(angle = 45, hjust = 0.01,family = "sans"))

facet_tile_1_GG <-
  ggplot(
    subset(full_melted_df, full_melted_df$facet_var == 1),
    aes(x = Plasmodium.species, y = variable)
  ) +
  geom_tile(aes(fill = color1), color = "black", size = 0.2) +
  geom_point(
    data = subset(meltedconflict_df, meltedconflict_df$facet_var == 1 & meltedconflict_df$point1 != "Square"),
    aes(x = Plasmodium.species, y = variable, shape = point1, color = color1), size = 32.5
  ) +
  geom_point(
    data = subset(meltedconflict_df, meltedconflict_df$facet_var == 1 & meltedconflict_df$point1 != "Square"),
    aes(y = variable, x = Plasmodium.species, shape = point2, color = color2), size = 32.5
  )  +
  scale_shape_manual(values = c("\u25E4", "\u25E2")) +
  scale_color_manual(values = c("All" = "#edce86", "Immature" = "#cf5982", "Mature" = "#009392")) +
  scale_fill_manual(values = c(
    "Immature" = "#cf5982",
    "Mature" = "#009392",
    "Unknown" = "white", "No" = "black", "All" = "#edce86"
  )) +
  scale_y_discrete(label = c("Greater burst size in...", "Inferred preference", "Known preference"), limits = rev) +
  scale_x_discrete(position = "top") +
  xlab("") +
  ylab("") +
  coord_equal() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.01),
    axis.ticks = element_blank(),
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_text(color = "black", size = 12)
  )

facet_tile_2_GG <- ggplot(
  subset(full_melted_df, full_melted_df$facet_var == 2),
  aes(x = Plasmodium.species, y = variable)
) +
  geom_tile(aes(fill = color1), color = "black", size = 0.2) +
  geom_point(
    data = subset(meltedconflict_df, meltedconflict_df$facet_var == 2 & meltedconflict_df$point1 != "Square"),
    aes(x = Plasmodium.species, y = variable, shape = point1, color = color1), size = 32.5
  ) +
  geom_point(
    data = subset(meltedconflict_df, meltedconflict_df$facet_var == 2 & meltedconflict_df$point1 != "Square"),
    aes(y = variable, x = Plasmodium.species, shape = point2, color = color2), size = 32.5
  ) +
  
  scale_shape_manual(values = c("\u25E4", "\u25E2")) +
  scale_color_manual(values = c("All" = "#edce86", "Immature" = "#cf5982", "Mature" = "#009392")) +
  scale_fill_manual(values = c(
    "Immature" = "#cf5982",
    "Mature" = "#009392",
    "Unknown" = "white", "No" = "black", "All" = "#edce86"
  )) +
  scale_y_discrete(label = c("Greater burst size in...", "Inferred preference", "Known preference"), limits = rev) +
  scale_x_discrete(position = "top") +
  xlab("") +
  ylab("") +
  coord_equal() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.3),
    axis.ticks = element_blank(),
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_text(color = "black", size = 12)
  )

facet_tile_3_GG <- ggplot(
  subset(full_melted_df, full_melted_df$facet_var == 3),
  aes(x = Plasmodium.species, y = variable)
) +
  geom_tile(aes(fill = color1), color = "black", size = 0.2) +
  geom_point(
    data = subset(meltedconflict_df, meltedconflict_df$facet_var == 3 & meltedconflict_df$point1 != "Square"),
    aes(x = Plasmodium.species, y = variable, shape = point1, color = color1), size = 32.5
  ) +
  geom_point(
    data = subset(meltedconflict_df, meltedconflict_df$facet_var == 3 & meltedconflict_df$point1 != "Square"),
    aes(y = variable, x = Plasmodium.species, shape = point2, color = color2), size = 32.5
  ) +
  scale_shape_manual(values = c("\u25E4", "\u25E2")) +
  scale_color_manual(values = c("All" = "#edce86", "Immature" = "#cf5982", "Mature" = "#009392")) +
  scale_fill_manual(values = c(
    "Immature" = "#cf5982",
    "Mature" = "#009392",
    "Unknown" = "white", "No" = "black", "All" = "#edce86"
  )) +
  scale_y_discrete(label = c("Greater burst size in...", "Inferred preference", "Known preference"), limits = rev) +
  scale_x_discrete(position = "top") +
  xlab("") +
  ylab("") +
  coord_equal() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0),
    axis.ticks = element_blank(),
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_text(color = "black", size = 12)
  )


facet_tile_4_GG <- ggplot(
  subset(full_melted_df, full_melted_df$facet_var == 4),
  aes(x = Plasmodium.species, y = variable)
) +
  geom_tile(aes(fill = color1), color = "black", size = 0.2) +
  geom_point(
    data = subset(meltedconflict_df, meltedconflict_df$facet_var == 4 & meltedconflict_df$point1 != "Square"),
    aes(x = Plasmodium.species, y = variable, shape = point1, color = color1), size = 45.5
  ) +
  geom_point(
    data = subset(meltedconflict_df, meltedconflict_df$facet_var == 4 & meltedconflict_df$point1 != "Square"),
    aes(y = variable, x = Plasmodium.species, shape = point2, color = color2), size = 45.5
  ) +
  scale_shape_manual(values = c("\u25E4", "\u25E2")) +
  scale_color_manual(values = c("All" = "#edce86", "Immature" = "#cf5982", "Mature" = "#009392")) +
  scale_fill_manual(values = c(
    "Immature" = "#cf5982",
    "Mature" = "#009392",
    "Unknown" = "white", "No" = "black", "All" = "#edce86"
  )) +
  scale_y_discrete(label = c("Greater burst size in...", "Inferred preference", "Known preference"), limits = rev) +
  scale_x_discrete(position = "top") +
  xlab("") +
  ylab("") +
  coord_equal() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.3),
    axis.ticks = element_blank(),
    legend.position = "none",
    axis.line = element_blank(),
    axis.text = element_text(color = "black", size = 12)
  )

facet_tile_1_GG/
facet_tile_2_GG/
facet_tile_3_GG/
facet_tile_4_GG

ggsave(here("Figures", "Raw", "ggtitle_Figure4_facetb_2.pdf"), width = 30, height = 20, units = "in")
