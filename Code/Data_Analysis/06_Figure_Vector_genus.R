#This is the code to make the figure for the maximum observed burst
#size and the vector genus

library(here)
source(here("Code", "Functions", "FUNC_Package_Loader.R"))
source(here("Code", "Functions", "FUNC_DA_full_data_loader.R"))

# Extract the necessary columns from mal_dat_asex_full
mal_dat_asex_vector <- mal_dat_asex_full[, c(
        "Plasmodium.species",
        "Vector_Genus",
        "OrderHost",
        "Experimental_Suspected_Vector_Genus",
        "Upper"
)]

# Separate rows by Vector_Genus and Experimental_Suspected_Vector_Genus
mal_dat_asex_vector_sep <- separate_rows(
        mal_dat_asex_vector, 
        "Vector_Genus", 
        "Experimental_Suspected_Vector_Genus"
)

# Remove rows where both Vector_Genus and Experimental_Suspected_Vector_Genus are NA
mal_dat_asex_vector_sep <- mal_dat_asex_vector_sep[
        !(is.na(mal_dat_asex_vector_sep$Vector_Genus) & 
                  is.na(mal_dat_asex_vector_sep$Experimental_Suspected_Vector_Genus)), 
]

# Melt the data, keeping the specified columns as ID variables
mal_dat_asex_vector_sep_melted <- na.omit(melt(
        mal_dat_asex_vector_sep, 
        id.vars = c("Plasmodium.species", "Upper", "OrderHost")
))

# Subset to include only known vectors and exclude 'fallax' species
mal_dat_asex_known_vector <- subset(
        mal_dat_asex_vector_sep_melted,
        mal_dat_asex_vector_sep_melted$variable %in% c("Vector_Genus") &
                mal_dat_asex_vector_sep_melted$Plasmodium.species != "fallax"
)

# Reorder factor levels for Vector_Genus
mal_dat_asex_known_vector$value <- factor(
        mal_dat_asex_known_vector$value,
        levels = c("Anopheles", "Aedes", "Coquillettidia", "Culex", "Culiseta", "Lutzomyia")
)

# Plot the data
ggplot(mal_dat_asex_known_vector, aes(x = value, y = Upper)) +
        geom_point(aes(color = OrderHost), size = 3) +
        geom_boxplot(alpha = 0.3) +
        scale_color_manual(values = c(
                'avian' = "#30d5c8", 
                'mammal' = "#fe64a3", 
                'reptile' = "#424b54"
        )) +
        xlab("Vector genus") +
        ylab("Maximum observed burst size") +
        theme_classic() +
        theme(
                legend.position = "none",
                axis.text = element_text(size = 14, color = "black"),
                axis.title = element_text(size = 15, color = "black")
        )

# Save the plot
ggsave(here("Figures", "Raw", "Vector_genus.pdf"), units = 'in', width = 8, height = 4)
