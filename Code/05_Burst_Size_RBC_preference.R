library(here)
library(ggplot2)
library(ggbeeswarm)
###Malaria Data - Species
mal_dat <- read.csv(here("Data","MALARIA_PAK_SPECIES.csv"))

###Subset the data
subsetted_mal_dat <-   mal_dat[,c("Plasmodium.species",
                                  "Average",
                                  "Lower",
                                  "Upper",
                                  "RBC_preference",    
                                  "Duration",
                                  "OrderHost")]

subsetted_mal_dat_preference <- subset(subsetted_mal_dat,
                                      subsetted_mal_dat$RBC_preference != " " &
                                      subsetted_mal_dat$RBC_preference != ""  )


subsetted_mal_dat_preference$RBC_preference <- 
  factor(subsetted_mal_dat_preference$RBC_preference,
         levels = c("Retics","Mature","All"))

ggplot(subsetted_mal_dat_preference , 
       aes(x = RBC_preference, y = Upper, fill = RBC_preference)) + 
       geom_point(shape = 21, size = 5)+
       scale_fill_viridis(option = 'mako', discrete = TRUE)+
       xlab("RBC preference") +
       ylab("Upper burst size") +
       theme_classic() +
       theme(legend.position = 'none',
        axis.text= element_text(size = 15, color = 'black'),
        axis.title = element_text(size = 16, color = 'black'))


ggsave(here("Figure","Data_Analysis",
            "Upper_Burst_Size_Preference_Figure.pdf"),
       width = 8, height = 6, units = 'in')
