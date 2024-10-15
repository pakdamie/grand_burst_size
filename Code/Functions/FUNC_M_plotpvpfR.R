plot_PC_PF_comparison <- function(data_frame, xlab, title ){
        
        ggplot(data_frame, aes(
        x = data_frame[,14],
        y = B_V, 
        group = species))+
                
        geom_line(aes(linetype= species), size = 0.9) +
        geom_point(
                data = subset(
                        data_frame,
                   data_frame$controlornot == "control"
                ),
                x = subset(
                        data_frame,
                        data_frame$controlornot == "control"
                )[,14],
                y = subset(
                        data_frame,
                        data_frame$controlornot == "control"
                )$B_V, 
                aes(shape = species), size = 3
        ) +
        scale_linetype_manual(name = "", values = c(1,3)) +
        theme_classic() +
        xlab(xlab) +
        ylab("Optimal burst size") +
        theme(legend.position = 'none',
                axis.text = element_text(color = "black", size = 13),
                axis.title = element_text(color = "black", size = 14)
        ) + 
        ylim(0,100)
        
        
}