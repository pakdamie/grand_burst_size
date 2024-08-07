###This is the function that plots out the 
### surface plot of the species of interest.


MAIN_SURFACEPLOT_GG_GRAPHER_FIT <- function(x, species){

    ###Produces the mortality and non-establishing infection lines
  horizontal_vert_df <- grapher_mortality_boundary(x)
  
  ###Find the optimum_point-
  optimum_strategy <- x[which.max(x$end_fitness),]
  

  GG_Fitness_Cut_PC <-
    ggplot(
      subset(
        x,
        x$status !=
          "Fail"
      ),
      aes(x = B_V, y = C_V)
    ) +
    geom_raster(aes(fill = end_fitness)) +
    geom_raster(
      data = subset(
        x,
        x$status ==
          "Fail"
      ),
      aes(x = B_V,y = C_V), fill = "#d1dbe8"
    ) +
    geom_segment(
      data = horizontal_vert_df,
      aes(
        x = x, xend = xend,
        y = y, yend = yend,
        color = id
      ), size = 1.1,
      lineend = "round"
    ) +
    scale_color_manual(
      values = c(
        "fail" = "black",
        "mort" = "#b8fcd5"
      ),
      guide = "none"
    ) +
    scale_fill_viridis(
      name = "Cumulative transmission \npotential",
      option = "magma"
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlab(expression(paste("Burst size"))) +
    ylab("Transmission investment (c)") +
    theme(
      text = element_text(size = 14),
      axis.text = element_text(size = 14, color = "black"),
      axis.title = element_text(size = 14, color = "black"),
      legend.position = "top"
    ) +
    geom_point(data = optimum_strategy ,
               aes( x = B_V, y= C_V),
               color = '#FF116B', size = 2)+
    geom_segment(
      data = optimum_strategy,
      aes(
        x = B_V, xend = B_V, y = 0.01, yend = C_V)
      , col = "#FF116B",
      linetype = 2) +
    geom_segment(data = optimum_strategy ,
                 aes(
                   x =1, xend = B_V, y = C_V, yend = C_V), col = "#FF116B",
                 linetype = 2) +
    
    annotate("text",
             x = 8, y = 0.93, label = "Unestablished \ninfection",
             size = 5
    ) +
    annotate("text",
             x = 45, y = 0.1, label = "Host \nMortality",
             size = 5, color = "#b8fcd5"
    )

  GG_Fitness_Cut_PC
  
  ggsave(here("Figures", "Raw", paste("02_Fitness_SurfacePlot",species,".pdf",sep = "_")),
         width = 8, height = 7.5, unit = "in")
  
  
  print(GG_Fitness_Cut_PC) 
}
