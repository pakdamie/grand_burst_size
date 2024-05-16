### This is the script for analyzing the phylogenetic signal in the maximum
### observed burst in either the host or parasite phylogenies.

library(here)
source(here("Code", "Functions", "FUNC_full_data_loader.R"))

###
species_clade_differentiatior <- mal_dat_asex_full[,c("Plasmodium.species","OrderHost", "Type_Host")]

# each observation gets its own unique id
mal_dat_asex$obs <- c(seq(1, nrow(mal_dat_asex)))
mal_dat_asex$Host_name <- as.factor(mal_dat_asex$Host)
mal_dat_asex$Parasite_name <- as.factor(mal_dat_asex$Plasmodium_species)
mal_dat_asex$Upper_Burst_Size <- as.integer(mal_dat_asex$Upper_Burst_Size)

###Reptile Subset

mal_dat_asex_full <- left_join(mal_dat_asex,species_clade_differentiatior, by = c("Plasmodium_species" = "Plasmodium.species") )
norep_mal_dat_asex_full <- subset(mal_dat_asex_full, mal_dat_asex_full$OrderHost != 'reptile')
reptile_names_P <- as.character(unique(subset(mal_dat_asex_full, mal_dat_asex_full$OrderHost == 'reptile')$Parasite_name))
reptile_names_H <- as.character(unique(subset(mal_dat_asex_full, mal_dat_asex_full$OrderHost == 'reptile')$Host_name))

plasmodium_tree_full_no_rep <- drop.tip(plasmodium_tree_full, reptile_names)
type_host_tree_subsetted_no_rep <- drop.tip(type_host_tree_subsetted, reptile_names_H)

### We calculate the branch length and the covariance matrix.
vcv_Plasmodium_no_rep <- vcv(compute.brlen(plasmodium_tree_full_no_rep ))
vcv_Host_no_rep <- vcv(compute.brlen(type_host_tree_subsetted_no_rep))

### Standardize the covariance matrix
vcv_Plasmodium_standard_no_rep <- vcv_Plasmodium_no_rep/ (det(vcv_Plasmodium_no_rep)^(1 / nrow(vcv_Plasmodium_no_rep)))
vcv_Host_standard_no_rep <-vcv_Host_no_rep / (det(vcv_Host_no_rep)^(1 / nrow(vcv_Host_no_rep)))

##### ECO + EVO
model_ecophylo_norep <- brm(
  Upper_Burst_Size ~ 1 +
    (1 | gr(Plasmodium_species, cov = vcv_Plasmodium_standard_no_rep)) +
    (1 | gr(Host, cov = vcv_Host_standard_no_rep)) +
    (1 | Host_name) +
    (1 | Parasite_name) +
    (1 | obs),
  prior = c(
    prior(student_t(4, 0, 1), "sd", lb = 0)
  ),
  data = norep_mal_dat_asex_full ,
  family = poisson("log"),
  warmup = 2000,
  iter = 4000,
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 11
  ),
  data2 = list(
    vcv_Plasmodium_standard_no_rep= vcv_Plasmodium_standard_no_rep,
    vcv_Host_standard_no_rep = vcv_Host_standard_no_rep
  ),
  save_pars = save_pars(all = TRUE)
)

fit_model_ecophylo <- add_criterion(model_ecophylo, "loo",
                                    reloo =
                                      TRUE
)