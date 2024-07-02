Plot_Simulation<- function(df_sim, title, variable_interest){
  plot_gg <- ggplot(df_sim, aes(x = !!sym(variable_interest),
                                y= B_V, group = species, color = species))+
    geom_line(linewidth = 1.2) + 
    geom_point()+
    theme_classic()+
    xlab(title)+
    ylab("Optimal burst size")+
    theme(axis.text = element_text(color = 'black', size = 14))
  
  return(plot_gg)
}
