###This script is simply for reporting 
library(here)
source(here("Code","Functions"," FUNC_full_data_loader.R"))

###Total number of Plasmodium species across the orders
table(mal_dat_asex_full[,"OrderHost"])

###Total number of Plasmodium subgenuses
View(table(mal_dat_asex_full[,c( "Subgenus")]))

##Total number of hosts
View(table(mal_dat_asex_full[,c( "Type_Host")]))
