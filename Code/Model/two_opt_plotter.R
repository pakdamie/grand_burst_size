ALL_OPT_TWO_PC <- subset(ALL_OPT_TWO, ALL_OPT_TWO$species == "PC")



ALL_OPT_TWO_PF <- subset(ALL_OPT_TWO, ALL_OPT_TWO$species == "PF")

ALL_OPT_TWO_PC$B_V_Change <- ALL_OPT_TWO_PC$B_V - 15.5
ALL_OPT_TWO_PF$B_V_Change <- ALL_OPT_TWO_PF$B_V - 17.0

ALL_OPT_FULL <- rbind(ALL_OPT_TWO_PC, ALL_OPT_TWO_PF)

ALL_OPT_FULL$change <- ifelse(ALL_OPT_FULL$B_V_Change < 0, 'decrease', 'increase' )




ggplot(ALL_OPT_FULL  , aes(x = variable_interest_1, y= variable_interest_2))+
         geom_tile(aes(fill = B_V_Change), size = 1, color = 'black')+ 
  facet_wrap(~species)+ scale_fill_divergent(high = '#921a37', low = '#3f76a5',
                                             mid = 'white', limits = c(-35,35))+
  scale_color_manual(values=c('decrease' = 'blue', 'increase' = 'red'))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  theme(panel.background= element_rect(fill = 'black'),
        panel.grid = element_blank())+
  coord_equal()
       
  
