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

### Is there a phylogenetic signal in the asexual duration?

model_parasites_duration<- brm(
  Upper_Burst_Size ~ Duration +
    (1|gr(Plasmodium_species , cov = vcv_Plasmodium_standard))+
    (1|gr(Host , cov = vcv_Host_standard))+
    (1|Host_name)+
    (1|Parasite_name)+
    (1|obs),
  data = mal_dat_asex ,
  family = poisson('log'), 
  prior = c(
    prior(student_t(4, 0, 1), "sd",lb = 0, ub = 3)),
  warmup = 2000,
  iter = 15000,
  control = list(adapt_delta = 0.999,
                 max_treedepth =15),
  data2 = list(vcv_Plasmodium_standard = vcv_Plasmodium_standard,
               vcv_Host_standard = vcv_Host_standard),
  save_pars = save_pars(all = TRUE))

save(model_parasites_duration, file = here("Output", "brms_output", "model_parasites_duration.RData"))

