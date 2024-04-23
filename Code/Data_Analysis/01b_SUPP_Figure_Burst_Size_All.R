### THIS IS FOR THE SUPPLEMENTARY PLOTS

###AVIAN PLOT
ggplot(
  Ordered_Ave_Species,
  aes(
    x = Upper,
    y = Plasmodium.species,
    fill = Subgenus
  )
) +
  geom_vline(xintercept = c(10, 20, 30), linetype = 3) +
  geom_segment(
    aes(
      x = Lower,
      xend = Upper,
      y = Plasmodium.species,
      yend = Plasmodium.species,
      color = Subgenus
    )
  ) +
  geom_point(
    shape = 21,
    size = 3
  ) +
  xlab("Burst size") +
  ylab("") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  scale_fill_manual(values = c(
    "Bennettinia" = "#FFB713",
    "Giovannolaia" = "#071952",
    "Haemamoeba" = "#A5B5DF",
    "Huffia" = "#088395",
    "Novyella" = "#35A29F",
    "Papernaia " = "#4F709C",
    "vaughani" = "#E5D283",
    "NA" = "grey"
  )) +
  scale_color_manual(values = c(
    "Bennettinia" = "#FFB713",
    "Giovannolaia" = "#071952",
    "Haemamoeba" = "#A5B5DF",
    "Huffia" = "#088395",
    "Novyella" = "#35A29F",
    "Papernaia " = "#4F709C",
    "vaughani" = "#E5D283",
    "NA" = "grey"
  )) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      size = 14,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 12,
      face = "italic",
      color = "black"
    ),
    axis.title = element_text(size = 15)
  )

ggsave(
  here(
    "Figures",
    "Raw", "01_Avian_burstsize_host_order_RAW.pdf"
  ),
  width = 8, height = 12, units = "in"
)


### Mammal PLOT###
mal_mammal_GG <-
  ggplot(
    Ordered_Mam_Species,
    aes(
      x = Upper,
      y = Plasmodium.species,
      fill = Subgenus
    )
  ) +
  geom_vline(xintercept = c(10, 20, 30), linetype = 3) +
  geom_segment(
    aes(
      x = Lower,
      xend = Upper,
      y = Plasmodium.species,
      yend = Plasmodium.species,
      color = Subgenus
    )
  ) +
  geom_point(
    shape = 21,
    size = 3
  ) +
  xlab("Burst size") +
  ylab("") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  scale_fill_manual(values = c(
    " " = "grey",
    "Laverania" = "#a86462",
    "Plasmodium" = "#bd302c",
    "Vinckeia" = "#560D0D"
  )) +
  scale_color_manual(values = c(
    " " = "grey",
    "Laverania" = "#a86462",
    "Plasmodium" = "#bd302c",
    "Vinckeia" = "#560D0D"
  )) +
  theme_classic() +
  theme(
    axis.text = element_text(
      size = 12,
      face = "italic",
      color = "black"
    ),
    axis.title = element_text(size = 14)
  )
mal_mammal_GG

ggsave(
  here(
    "Figures",
    "Raw", "02_Mammal_burstsize_host_order.pdf"
  ),
  width = 8, height = 12, units = "in"
)



###REPTILE
Ordered_rep_Species$facet <- ifelse(Ordered_rep_Species$Indivdiual_Number < 55, 1, 2)


### REPTILE PART 1

ggplot(
  subset(
    Ordered_rep_Species,
    Ordered_rep_Species$facet == 1
  ),
  aes(
    x = Upper,
    y = Plasmodium.species, fill = Subgenus
  )
) +
  geom_vline(xintercept = c(10, 20, 30, 50, 100), linetype = 3) +
  geom_segment(
    aes(
      x = Lower,
      xend = Upper,
      y = Plasmodium.species,
      yend = Plasmodium.species,
      color = Subgenus
    )
  ) +
  geom_point(
    shape = 21,
    size = 3
  ) +
  facet_wrap(~facet, scales = "free_y") +
  xlab("Burst size") +
  ylab("") +
  scale_x_continuous(limits = c(0, 130), breaks = seq(0, 130, 10)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c(
    "Asiamoeba" = "#416D19",
    "Carinamoeba" = "#4CCD99",
    "Garnia" = "#007F73",
    "Lacertamoeba" = "#DA0C81",
    "Ophidiella" = "#C5E898",
    "Paraplasmodium" = "#83C0C1",
    "Sauramoeba" = "#6962AD",
    "NA" = "grey"
  )) +
  scale_color_manual(values = c(
    "Asiamoeba" = "#416D19",
    "Carinamoeba" = "#4CCD99",
    "Garnia" = "#007F73",
    "Lacertamoeba" = "#DA0C81",
    "Ophidiella" = "#C5E898",
    "Paraplasmodium" = "#83C0C1",
    "Sauramoeba" = "#6962AD",
    "NA" = "grey"
  )) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      size = 14,
      color = "black"
    ),
    axis.title = element_text(size = 15),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

ggsave(
  here(
    "Figures",
    "Raw", "03_Reptile_burstsize_host_order_part1.pdf"
  ),
  width = 10, height = 12, units = "in"
)




### REPTILE PART 2

ggplot(
  subset(
    Ordered_rep_Species,
    Ordered_rep_Species$facet == 2
  ),
  aes(
    x = Upper,
    y = Plasmodium.species, fill = Subgenus
  )
) +
  geom_vline(xintercept = c(10, 20, 30, 50, 100), linetype = 3) +
  geom_segment(
    aes(
      x = Lower,
      xend = Upper,
      y = Plasmodium.species,
      yend = Plasmodium.species,
      color = Subgenus
    )
  ) +
  geom_point(
    shape = 21,
    size = 3
  ) +
  facet_wrap(~facet, scales = "free_y") +
  xlab("Burst size") +
  ylab("") +
  scale_x_continuous(limits = c(0, 130), breaks = seq(0, 130, 10)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c(
    "Asiamoeba" = "#416D19",
    "Carinamoeba" = "#4CCD99",
    "Garnia" = "#007F73",
    "Lacertamoeba" = "#DA0C81",
    "Ophidiella" = "#C5E898",
    "Paraplasmodium" = "#83C0C1",
    "Sauramoeba" = "#6962AD",
    "NA" = "grey"
  )) +
  scale_color_manual(values = c(
    "Asiamoeba" = "#416D19",
    "Carinamoeba" = "#4CCD99",
    "Garnia" = "#007F73",
    "Lacertamoeba" = "#DA0C81",
    "Ophidiella" = "#C5E898",
    "Paraplasmodium" = "#83C0C1",
    "Sauramoeba" = "#6962AD",
    "NA" = "grey"
  )) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      size = 14,
      color = "black"
    ),
    axis.title = element_text(size = 15),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

ggsave(
  here(
    "Figures",
    "Raw", "03_Reptile_burstsize_host_order_part2.pdf"
  ),
  width = 10, height = 12, units = "in"
)
