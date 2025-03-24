
library(ncdf4)
library(dplyr)

# Définir le dossier contenant les fichiers .nc
folder_path <- "precip_nc"

# Lister tous les fichiers .nc du dossier
files <- list.files(folder_path, pattern = "\\.nc$", full.names = TRUE)

# Initialiser une liste pour stocker les données
all_data <- list()

# Boucle pour lire et extraire les données de chaque fichier .nc
for (file in files) {
  nc <- nc_open(file)  # Ouvrir le fichier
  
  # Liste des variables disponibles
  print(nc$var)  # Vérifier les noms des variables pour les extraire
  
  # Extraction des variables importantes (exemple: température, humidité)
  temperature <- ncvar_get(nc, "temperature")  # Adapter le nom de la variable
  humidity <- ncvar_get(nc, "humidity")  # Exemple
  
  # Extraction des coordonnées spatiales/temporelles (si présentes)
  time <- ncvar_get(nc, "time")  # Vérifier si la variable "time" existe
  lat <- ncvar_get(nc, "latitude")
  lon <- ncvar_get(nc, "longitude")
  
  # Fermer le fichier après lecture
  nc_close(nc)
  
  # Transformer les données en data frame
  df <- data.frame(file = basename(file), time, lat, lon, temperature, humidity)
  
  # Ajouter au dataset global
  all_data[[file]] <- df
}

# Fusionner toutes les données en un seul dataset
final_dataset <- bind_rows(all_data)

# Sauvegarder en CSV
write.csv(final_dataset, "dataset_dengue.csv", row.names = FALSE)

# Afficher un aperçu du fichier final
head(final_dataset)
