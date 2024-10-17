### This is the script for analyzing the phylogenetic signal in the maximum
### observed burst in either the host or parasite phylogenies.
library(here)
library(here("Code", "Functions","FUNC_Package_Loader.R"))
source(here("Code", "Functions", "FUNC_DA_full_data_loader.R"))

mal_dat_asex_Subsetted <- subset(
  mal_dat_asex_full[, c("Plasmodium.species", 
                        "Upper", "Duration")])

duration_burst_size_GG <- ggplot(
  mal_dat_asex_Subsetted,
  aes(
    x = (Duration),
    y = Upper,
    group = Duration
  )
) + geom_point() + 
  geom_violin(alpha = 0.23, color = NA, fill = '#3D007F') +
  geom_beeswarm(size = 3.0, shape = 21, cex = 1.2, color = "NA") +
  scale_x_continuous(breaks = c(0, 24, 36, 48, 60, 72, 84, 96)) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 5)
  ) +
  xlab("Duration (hours)") +
  ylab("Maximum observed burst size") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 15, color = "black"),
    axis.title = element_text(size = 16, color = "black")
  ); duration_burst_size_GG 

ggsave(here("Figures", "Raw", "duration_ggplot.pdf"), 
       width =10, height = 9, 
       units = 'in')

