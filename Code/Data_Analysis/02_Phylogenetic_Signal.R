library(here)
source(here("Code","Functions", "FUNC_Package_Loader.R"))
source(here("Code", "Data_Analysis","00_parasite_phylogeny_tree.R"))
source(here("Code", "Data_Analysis","00_full_phylogeny_tree.R"))
###Use the subsetted onesource(here("Code", "Data_Analysis","00_parasite_phylogeny_tree.R"))

mal_dat_asex <- read.csv(here("Data","MALARIA_PHYLOGENY_HOST_VARIOUS.csv"))
mal_dat_asex$Host <- add_underscore(mal_dat_asex$Host)


###Pruning down the host trees for the analysis as well as making the cophylogenetic
###figure 

type_host_tree_subsetted <- drop.tip(Full_SuperTree_Host, 
                                     Full_SuperTree_Host$tip.label
                                     [-match(mal_dat_asex$Host, 
                                             Full_SuperTree_Host$tip.label)])

                                     
vcv_Plasmodium <- vcv(compute.brlen(plasmodium_tree_full))
vcv_TypeHost <- vcv(compute.brlen(type_host_tree_subsetted))


mal_dat_asex$obs <- seq(1, nrow(mal_dat_asex))
mal_dat_asex$Host_name <- as.factor(mal_dat_asex$Host)
mal_dat_asex$Parasite_name <- as.factor(mal_dat_asex$Plasmodium_species)


get_prior(Upper_Burst_Size ~ 1 +
            (1|gr(Plasmodium_species , cov = vcv_Plasmodium))+
            (1|gr(Host , cov = vcv_TypeHost))+
            (1|Host_name)+
            (1|Parasite_name) + 
            (1|obs),
          data = mal_dat_asex ,
          family = poisson,
          data2 = list(vcv_Plasmodium = vcv_Plasmodium,
                       vcv_TypeHost = vcv_TypeHost))











#####ECO + EVO 
model_null_ecophylo <- brm(
  Upper_Burst_Size ~ 1 +
    (1|gr(Plasmodium_species , cov = vcv_Plasmodium))+
    (1|gr(Host , cov = vcv_TypeHost))+
    (1|Host_name)+
    (1|Parasite_name) + 
    (1|obs),
     prior = c( 
                prior(student_t(3, 0,.05) ,"sd", lb = 0, ub = 4.5,  group = "Host" ),
                prior(student_t(3, 0,.05) ,"sd", lb = 0, ub = 4.5, group = "Host_name" ),
                prior(student(3, 0,0.05), "sd", lb = 0, ub = 4.5, group = 'obs'),
                prior(student_t(3, 1,.25) , "sd", lb = 0, ub = 4.5,  group = "Plasmodium_species"),
                prior(student_t(3, 1,.25) ,"sd", lb = 0, ub = 4.5,  group = "Parasite_name"),
                prior(student_t(3, 3, 2.5), "Intercept", lb = 0)),
  data = mal_dat_asex ,
  family = poisson, 
  warmup = 2000,
  iter = 10000,
  data2 = list(vcv_Plasmodium = vcv_Plasmodium,
               vcv_TypeHost = vcv_TypeHost),
  save_pars = save_pars(all = TRUE))

pairs(model_null_ecophylo)
plot(model_null_ecophylo)

check_loo_null_ecophylo <- loo(model_null_ecophylo,reloo = TRUE)
  plot(check_loo_null_ecophylo )

model_parasites_only_ecophylo <- brm(
  Upper_Burst_Size ~ 1 +
    (1|gr(Plasmodium_species , cov = vcv_Plasmodium))+
    (1|Parasite_name) + 
    (1|obs),
  prior = c( 
    prior(student_t(3, 1,.05) , "sd", lb = 0, ub = 4.5,  group = "Plasmodium_species"),
    prior(student_t(3, 1,.05) ,"sd", lb = 0, ub = 4.5,  group = "Parasite_name"),
    prior(student_t(3, 3, 2.5), "Intercept", lb = 0)),
  data = mal_dat_asex ,
  family = poisson, 
  warmup = 2000,
  iter = 10000,
  data2 = list(vcv_Plasmodium = vcv_Plasmodium,
               vcv_TypeHost = vcv_TypeHost),
  save_pars = save_pars(all = TRUE))


model_host_only_ecophylo <- brm(
  Upper_Burst_Size ~ 1 +
    (1|gr(Host , cov = vcv_TypeHost))+
    (1|Host_name)+
    (1|obs),
  prior = c( 
    prior(student_t(3, 0,.05) ,"sd", lb = 0, ub = 4.5,  group = "Host" ),
    prior(student_t(3, 0,.05) ,"sd", lb = 0, ub = 4.5, group = "Host_name" ),

    prior(student_t(3, 3, 2.5), "Intercept", lb = 0)),
  data = mal_dat_asex ,
  family = poisson, 
  warmup = 2000,
  iter = 15000,
  data2 = list(vcv_Plasmodium = vcv_Plasmodium,
               vcv_TypeHost = vcv_TypeHost),
  save_pars = save_pars(all = TRUE))


fit_orig <- add_criterion(model_null_ecophylo , "loo", reloo = TRUE)
fit_par <- add_criterion(model_parasites_only_ecophylo, "loo",reloo = TRUE)
fit_host <- add_criterion(model_host_only_ecophylo, "loo",reloo = TRUE)
loo_compare(fit_orig,fit_par ,fit_host,criterion = "loo")


###RUN LOO 

###USING THE VARIANCE - DECOMPOSITION (Hard to calculate the ICC)
###EVIDENCE 1
variance_decomposition(model_null) #This is saying that most of the model is explained
                                   #by the random effects
variance_decomposition(model_null, re_formula = ~(1|gr(Plasmodium_species , cov = vcv_Plasmodium)))

###The total variance is conditioned on The random effect Plasmodium_species- we're excluding
###the host random effect.
variance_decomposition(model_null, re_formula = ~(1|gr(Host , cov = vcv_TypeHost))) #Includes 0 so not signifcant


###EVIDENCE 2


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

###EVIDENCE 3 (BAD!!!!)
###ADD TRULY NULL WITH NOTHINGGGGGG
fit1 <- add_criterion(model_null_ecophylo , "loo")
fit2 <- add_criterion(model_null_parasite_only, "loo",moment_match = )
fit3 <- add_criterion(model_null_host_only, "loo")
loo_compare(fit1,fit2, fit3,criterion = "loo")
loo(fit1)

###EVIDENCE 4
bayes_R2(model_null)
bayes_R2(model_null_parasite_only)
bayes_R2(model_null_host_only)

###haha

###EVIDENCE 5
#Repeatability for Gaussian and non-Gaussian data: a practical guide 
#for biologists by Shinichi Nakagawa, Holger Schielzeth

Random_effect_H = 0.35^2
Random_effect_P = 1.48^2
B0 = 2.93


###Frequentist methodddd
Random_effect_H/(Random_effect_H + Random_effect_P + log(1/exp(B0)+1))
Random_effect_P/(Random_effect_H + Random_effect_P + log(1/exp(B0)+1))


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