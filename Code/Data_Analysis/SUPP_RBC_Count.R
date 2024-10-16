Mammal_trait_dat <- read.csv(here("Data","Mammal","Mammal_trait.csv"))
mammal_mal_dat <- subset(mal_dat_asex_full, mal_dat_asex_full$OrderHost == 'mammal')
mammal_mal_dat
colnames(library(here)
source(here("Code", "Functions", "FUNC_DA_full_data_loader.R"))
colnames(mammal_mal_dat)
plot(mammal_mal_dat$Body_mass_g, mammal_mal_dat$RBC_Host)
plot(mammal_mal_dat$Body_mass_g, na.omit(mammal_mal_dat$RBC_Host))
ggplot(mammal_mal_dat, aes(x = "Body_mass_g", y = "RBC_Host")) + geom_point()
ggplot(mammal_mal_dat, aes(x = Body_mass_g, y = RBC_Host)) + geom_point()
ggplot(mammal_mal_dat, aes(x = as.numeric(Body_mass_g), y = RBC_Host)) + geom_point()
ggplot(mammal_mal_dat, aes(x = as.numeric(Body_mass_g), y = as.numeric(RBC_Host))) + geom_point()
ggplot(mammal_mal_dat, aes(x = as.numeric(Body_mass_g), y = as.numeric(RBC_Host))) +
geom_point() + xlim(0,250000)
