# Charger les bibliothèques
library(dplyr)
library(tidyr)
library(readr)

# Charger les fichiers
df_climate <- read_csv("climate_change_1971-2021.csv")
df_dengue <- read_csv("dengue_data.csv")

# Transformer df_climate : convertir les colonnes d'années en une seule colonne "Year"
df_climate_long <- df_climate %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Temperature_Change") %>%
  mutate(Year = as.integer(Year))

# Harmoniser les noms de colonnes pour la fusion
df_dengue <- df_dengue %>%
  rename(Country = adm_0_name, ISO3 = ISO_A0) %>%
  select(ISO3, Year, dengue_total)

# Ajouter les années manquantes pour chaque pays
df_full_years <- expand.grid(ISO3 = unique(df_climate_long$ISO3), 
                             Year = unique(df_climate_long$Year))

# Fusionner avec df_dengue pour garantir toutes les années et remplacer NA par 0
df_dengue_complete <- df_full_years %>%
  left_join(df_dengue, by = c("ISO3", "Year")) %>%
  mutate(dengue_total = ifelse(is.na(dengue_total), 0, dengue_total))

# Fusionner avec df_climate_long
merged_df <- df_dengue_complete %>%
  left_join(df_climate_long, by = c("ISO3", "Year")) %>%
  distinct(ISO3, Year, .keep_all = TRUE)  # Suppression des doublons

# Sauvegarder le fichier sans doublons
write_csv(merged_df, "merged_data.csv")

# Afficher les premières lignes
head(merged_df)