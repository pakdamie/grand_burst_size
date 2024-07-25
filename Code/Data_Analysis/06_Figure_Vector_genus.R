library(here)
source(here("Code", "Functions", 'FUNC_Package_Loader.R'))
source(here("Code", "Functions", "FUNC_DA_full_data_loader.R"))


mal_dat_asex_vector <- mal_dat_asex_full[,c("Plasmodium.species",
                                            "Vector_Genus", 
                                            "Experimental_Suspected_Vector_Genus",
                                            "Upper")]

mal_dat_asex_vector_sep <- separate_rows(mal_dat_asex_vector,"Vector_Genus",
                                         "Experimental_Suspected_Vector_Genus")


mal_dat_asex_vector_sep <- mal_dat_asex_vector_sep[!(is.na(mal_dat_asex_vector_sep $Vector_Genus) == TRUE &
                        is.na(mal_dat_asex_vector_sep$Experimental_Suspected_Vector_Genus) == TRUE),]

mal_dat_asex_vector_sep_melted <- na.omit(melt(mal_dat_asex_vector_sep, id.vars = c("Plasmodium.species","Upper")))

ggplot(mal_dat_asex_vector_sep_melted , aes(x = value, y= Upper))+ geom_point(aes(color = variable),
                                                                                    size =3 )+
  geom_boxplot(alpha = 0.3)+theme_bw()+
  xlab("")+ ylab("Maximum observed burst size")
  theme(axis.text.x = element_text(size =12, color = 'black'))

  
###ONLY KNOWN
  
mal_dat_asex_known_vector <- subset(mal_dat_asex_vector_sep_melted,
                                    mal_dat_asex_vector_sep_melted$variable =="Vector_Genus"
                                    & mal_dat_asex_vector_sep_melted$Plasmodium.species != "fallax")
      


mal_dat_asex_known_vector$value <- factor(mal_dat_asex_known_vector$value,
                                          levels = c("Anopheles","Aedes","Coquillettidia", "Culex","Culiseta", "Lutzomyia"))
                      
ggplot(mal_dat_asex_known_vector, aes(x = value, y= Upper))+ geom_point(aes(color = variable),
                                                                              size =3 )+
  geom_boxplot(alpha = 0.3)+theme_classic()+
  xlab("")+ ylab("Maximum observed burst size")+
  
theme(legend.position = 'none',
  axis.text = element_text(size =14, color = 'black'))


###Open/closed- experimental/wild
### point style/color for host taxonomic groups

