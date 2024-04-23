### MAIN_FIGURER PLOT:

Ordered_Ave_Species$id <- "bird"
Ordered_Mam_Species$id <- "mammal"

Ave_Mam_Ordered_Species <- rbind(Ordered_Ave_Species, Ordered_Mam_Species)

AVE_MAM_NO_LABEL <- ggplot(
  Ave_Mam_Ordered_Species,
  aes(
    x = Upper,
    y = Plasmodium.species,
    color = id
  )
) +
  geom_vline(xintercept = c(10, 20, 30), linetype = 3) +
  geom_segment(
    aes(
      x = Lower,
      xend = Upper,
      y = Plasmodium.species,
      yend = Plasmodium.species,
    )
  ) +
  geom_point(
    size = 2.5
  ) +
  xlab("Burst size") +
  ylab("") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0, 130), breaks = seq(0, 130, 10)) +
  scale_color_manual(values = c("#30d5c8", "#fe64a3")) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      size = 14,
      color = "black"
    ),
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 15)
  )


REP_NO_LABEL <- ggplot(
    Ordered_rep_Species,
  aes(
    x = Upper,
    y = Plasmodium.species
  ),
  
) +
  geom_vline(xintercept = c(10, 20, 30, 50, 100), linetype = 3) +
  geom_segment(
    aes(
      x = Lower,
      xend = Upper,
      y = Plasmodium.species,
      yend = Plasmodium.species
    ),
    color = "#424b54"
  ) +
  geom_point(
    shape = 21,
    size = 3,
    fill = "#424b54"
  ) +
  xlab("Burst size") +
  ylab("") +
  scale_x_continuous(limits = c(0, 130), breaks = seq(0, 130, 10)) +
  scale_y_discrete(limits = rev) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      size = 14,
      color = "black"
    ),
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 15)
  )

REP_NO_LABEL + AVE_MAM_NO_LABEL 

ggsave(
  here(
    "Figures",
    "Raw", "Full_mam_ave_rep_BURST_SIZE.pdf"
  ),
  width = 14, height = 12, units = "in"
)
