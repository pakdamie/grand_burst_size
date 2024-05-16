load(here("Output", "brms_output", "fit_model_ecophylo.RData"))
load(here("Output", "brms_output", "fit_model_host_only.RData"))
load(here("Output", "brms_output", "fit_model_parasite_only.RData"))

fit_model_ecophylo_draws <- gather_draws(fit_model_ecophylo,
               sd_Host__Intercept,
               sd_Host_name__Intercept,
               sd_Parasite_name__Intercept,
               sd_Plasmodium_species__Intercept,
               ndraws = 8000)

fit_draws_GG <- ggplot(fit_model_ecophylo_draws,aes(x = (.variable), y= .value)) +
  stat_pointinterval(  .width = c(0.5, 0.95),
                       point_size = 10) + theme_classic() + 
  xlab("")+ ylab("Coefficient estimate")+
  scale_x_discrete(label = c(
                             "Parasite identity",
                             "Parasite phylogeny",
                             "Host identity", "
                             Host phylogeny"))
  
ggsave(here("Figures", "Raw", "fit_draws_GG.pdf"), width = 11,
       height =3, units = 'in')


data.frame_Intercept <- data.frame(coef(fit_model_ecophylo)[[5]])
data.frame_Intercept$Species <- rownames(data.frame_Intercept)

ggplot(data.frame_Intercept, aes(y = Species, x= exp(Estimate.Intercept)))+
  geom_point()



variance_partition <- model_null%>% 
  
  tidy_draws() %>% 
  dplyr::select(starts_with("sd_")) %>%
  transmute_all(.funs = list(sq = ~(. ^ 2))) %>% 
  mutate(total_var = rowSums(.)) %>%
  mutate_at(.vars = vars(-total_var), 
            .funs = list(pct = ~(. / total_var)))  %>% 
  map_df(.f = ~ median_hdci(., .width = 0.95), .id = "component") 

###Parasite variance over host variance - RATIO!

ggplot(variance_partition[4:5,], aes(x = y, y = component))+
  geom_point(size =4) + geom_segment(aes(x = ymin, xend=ymax, y=component, 
                                         yend=component))+
  scale_y_discrete(label=c("Host","Plasmodium"))+theme_classic()+
  xlab("Variance component")+
  ylab("")+
  theme(axis.text = element_text(size =14),
        axis.title = element_text(size = 15))

###EVIDENCE 5
#Repeatability for Gaussian and non-Gaussian data: a practical guide 
#for biologists by Shinichi Nakagawa, Holger Schielzeth

Random_effect_HName = 0.09 ^2
Random_effect_HPhy =  0.08^2
Random_effect_PName =  0.48^2
Random_effect_PPhy =  0.13^2
Random_effect_obs =  0.06^2
B0 = 2.93


###Frequentist methodddd
Random_effect_HName/(Random_effect_HName+ Random_effect_HPhy + Random_effect_PName + Random_effect_PPhy + Random_effect_obs+ log(1/exp(B0)+1))
Random_effect_HPhy /(Random_effect_HName+ Random_effect_HPhy + Random_effect_PName + Random_effect_PPhy + Random_effect_obs+ log(1/exp(B0)+1))
Random_effect_PName /(Random_effect_HName+ Random_effect_HPhy + Random_effect_PName + Random_effect_PPhy + Random_effect_obs+ log(1/exp(B0)+1))
Random_effect_PPhy/(Random_effect_HName+ Random_effect_HPhy + Random_effect_PName + Random_effect_PPhy + Random_effect_obs+ log(1/exp(B0)+1))


###Hypothesis testing from BRMS - 
hypothesis(
  model_null,"(sd_Plasmodium_species__Intercept^2 )/
  (sd_Host__Intercept^2 + sd_Plasmodium_species__Intercept^2 
  + log(1/exp(b_Intercept)+1)) > 0 ", class = NULL)


hypothesis(
  model_null,"(sd_Host__Intercept^2 )/
  (sd_Host__Intercept^2 + sd_Plasmodium_species__Intercept^2 
  + log(1/exp(b_Intercept)+1)) > 0 ", class = NULL)

###PARASITES PHYLOGENY MATTER MORE