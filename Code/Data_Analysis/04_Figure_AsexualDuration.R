### This is the script for analyzing the phylogenetic signal in the maximum
### observed burst in either the host or parasite phylogenies.
library(here)
source(here("Code", "Functions", "FUNC_full_data_loader.R"))

mal_dat_asex_Subsetted <- subset(mal_dat_asex_full[, c("Plasmodium.species", "Upper", "Duration")])

duration_burst_size_GG <- ggplot(
  mal_dat_asex_Subsetted,
  aes(
    x = (Duration),
    y = Upper,
    fill = Duration,
    group = Duration
  )
) +
  geom_violin(alpha = 0.2, color = NA) +
  geom_beeswarm(size = 2.5, shape = 21, cex = 1.2, color = "NA") +
  scale_fill_viridis(option = "turbo") +
  scale_x_continuous(breaks = c(0, 24, 36, 48, 60, 72, 84, 96)) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 5)
  ) +
  xlab("Duration (Hours)") +
  ylab("Maximum observed burst size") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 15, color = "black"),
    axis.title = element_text(size = 16, color = "black")
  )

ggsave(here("Figures", "Raw", "duration_ggplot.pdf"), width =10, height = 9, 
       units = 'in')
