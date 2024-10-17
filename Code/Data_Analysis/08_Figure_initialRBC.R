#This is the script for making the initial red blood cells 
# and the maximum observed burst size

source(here("Code", "Functions", "FUNC_DA_full_data_loader.R"))

#We only have data of the initial RBC for mammals
mammal_mal_dat <- subset(mal_dat_asex_full, 
  mal_dat_asex_full$OrderHost == 'mammal')


data_RBC_GG <- ggplot(mammal_mal_dat, 
  aes(x =(RBC_Host * 10^6), y= (Upper))) + 
  geom_point(size = 1.5, color = "#fe64a3") + 
  xlab(expression("Average RBCs ("*mu*"L"^-1*")")) +
 ylab("Maximum observed burst size") + 
  scale_linetype_manual(name = '', values = c(1,2)) +
  scale_x_log10(
    limits = c(10^6.25, 10^7.75),
    breaks = 10^seq(6.25, 7.75, by = 0.25),  # From 10^5 to 10^8 in steps of 0.5
    labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  scale_y_continuous(limits = c(0,100))+
  theme_classic() +
  theme(axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'))

data_RBC_GG 
ggsave(here("Figures", "Raw", "initial_rbc_ggplot.pdf"), 
       width = 10, height = 9, 
       units = 'in')
