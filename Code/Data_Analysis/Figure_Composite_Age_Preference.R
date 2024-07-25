###FULL FIGURE of the age_preference in RBC

source(here("Code","Data_Analysis","05_Figure_Burst_Size_RBC_preference.R"))
source(here("Code", "Data_Analysis","07_Figure_Reticulocytes.R"))




(Preference_Known_GG /((Mammal_Mass_Retic_GG + Upper_GG )))


ggsave(here("Figures", "Raw", "FULL_preference_Figure.pdf"),
       height = 10,
       width = 12, units = "in"
)
