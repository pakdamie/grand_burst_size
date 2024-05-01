### This is the script for analyzing the phylogenetic signal in the maximum
###observed burst in either the host or parasite phylogenies.

library(here)
source(here("Code","Functions","FUNC_full_data_loader.R"))

###We calculate the branch length and the covariance matrix.
vcv_Plasmodium <- vcv(compute.brlen(plasmodium_tree_full))
vcv_Host <- vcv(compute.brlen(type_host_tree_subsetted))

###Standardize the covariance matrix
vcv_Plasmodium_standard <- vcv_Plasmodium /(det(vcv_Plasmodium )^(1/nrow(vcv_Plasmodium)))
vcv_Host_standard <- vcv_Host  /(det(vcv_Host  )^(1/nrow(vcv_Host )))

#each observation gets its own unique id
mal_dat_asex$obs <- c(seq(1, nrow(mal_dat_asex))) 
mal_dat_asex$Host_name <- as.factor(mal_dat_asex$Host)
mal_dat_asex$Parasite_name <- as.factor(mal_dat_asex$Plasmodium_species)
mal_dat_asex$Upper_Burst_Size <- as.integer(mal_dat_asex$Upper_Burst_Size)


#####ECO + EVO 
model_ecophylo <- brm(
  Upper_Burst_Size ~ 1 +
      (1 | gr(Plasmodium_species, cov = vcv_Plasmodium_standard)) +
      (1 | gr(Host, cov = vcv_Host_standard)) +
      (1 | Host_name) +
      (1 | Parasite_name) +
      (1 | obs),
  prior = c(
    prior(student_t(4, 0, 1), "sd",lb = 0)),
  data = mal_dat_asex,
  family = poisson("log"),
  warmup = 2000,
  iter = 4000,
  control = list(adapt_delta = 0.999,
                 max_treedepth = 11),
  data2 = list(
    vcv_Plasmodium_standard = vcv_Plasmodium_standard,
    vcv_Host_standard = vcv_Host_standard),
  save_pars = save_pars(all = TRUE)
)

fit_model_ecophylo  <- add_criterion(model_ecophylo , "loo", reloo =
                                     TRUE)
save(fit_model_ecophylo, file = here("Output", "brms_output", "fit_model_ecophylo.RData"))


###Host only
model_host_only_ecophylo <- 
  brm(Upper_Burst_Size ~ 1 +
         (1|gr(Host , cov = vcv_Host_standard))+
         (1|Host_name)+
         (1|obs),
       data = mal_dat_asex, family = poisson('log'), 
       warmup = 2000, 
       iter =4000, 
       prior = c(prior(student_t(4, 0, 1), "sd",lb = 0)),
       control = list(adapt_delta = 0.999),
       data2 =list(vcv_Plasmodium_standard = vcv_Plasmodium_standard,
                   vcv_Host_standard = vcv_Host_standard), 
       save_pars = save_pars(all = TRUE))

fit_model_host_only  <- add_criterion(model_host_only_ecophylo, "loo", reloo =
                                       TRUE)
save(fit_model_host_only , file = here("Output", "brms_output", "fit_model_host_only.RData"))


###Parasite only
model_parasites_only_ecophylo <- brm(
  Upper_Burst_Size ~ 1 +
    (1|gr(Plasmodium_species , cov = vcv_Plasmodium_standard))+
    (1|Parasite_name)+
    (1|obs),
  data = mal_dat_asex ,
  family = poisson('log'), 
  prior = c(
    prior(student_t(4, 0, 1), "sd",lb = 0)),
  warmup = 3000,
  iter = 10000,
  control = list(adapt_delta = 0.999,
                 max_treedepth = 15),
  data2 = list(vcv_Plasmodium_standard = vcv_Plasmodium_standard,
               vcv_Host_standard = vcv_Host_standard),
  save_pars = save_pars(all = TRUE))

fit_model_parasite_only  <- add_criterion(model_parasites_only_ecophylo, "loo", reloo =
                                        TRUE)
save(fit_model_parasite_only , file = here("Output", "brms_output", "fit_model_parasite_only.RData"))
