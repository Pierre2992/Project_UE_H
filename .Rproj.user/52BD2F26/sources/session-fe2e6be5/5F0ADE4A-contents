# Charger les bibliothèques nécessaires
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Charger les données fusionnées
merged_df <- read_csv("merged_data.csv")  # Remplace par le bon fichier si besoin

# Assurez-vous que merged_df contient une colonne avec les noms des pays
# Si ce n'est pas le cas, vous pouvez ajouter une colonne "Country" avec les noms des pays
# Exemple : merged_df <- merged_df %>% left_join(country_names_df, by = "ISO3")

# Fonction pour tracer l'histogramme avec superposition des cas de dengue
plot_temp_dengue <- function(country_code) {
  country_data <- merged_df %>% filter(ISO3 == country_code)
  
  # Calculer les maxima pour la normalisation
  max_temp <- max(country_data$Temperature_Change, na.rm = TRUE)
  max_dengue <- max(country_data$dengue_total, na.rm = TRUE)
  
  # Si max_dengue est 0, définir une valeur par défaut pour éviter les divisions par zéro
  if (max_dengue == 0) {
    max_dengue <- 1
  }
  
  ggplot(country_data, aes(x = Year)) +
    geom_col(aes(y = Temperature_Change, fill = "Température"), alpha = 0.7) +
    geom_line(aes(y = dengue_total / max_dengue * max_temp,
                  color = "Cas de dengue"), size = 1) +
    scale_y_continuous(
      name = "Température (°C)",
      sec.axis = sec_axis(~ . * max_dengue / max_temp,
                          name = "Nombre de cas de dengue")
    ) +
    labs(title = paste("Température et Cas de Dengue -", country_data$Country[1]),
         x = "Année") +
    scale_fill_manual(name = "", values = c("Température" = "blue")) +
    scale_color_manual(name = "", values = c("Cas de dengue" = "red")) +
    theme_minimal()
}

# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Visualisation des données de température et de dengue"),
  sidebarLayout(
    sidebarPanel(
      actionButton("btn_prev", "Précédent"),
      actionButton("btn_next", "Suivant"),
      textOutput("country_name")
    ),
    mainPanel(
      plotOutput("tempDenguePlot")
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  # Initialiser l'index du pays
  country_index <- reactiveVal(1)
  
  # Liste des codes ISO3 des pays
  country_codes <- unique(merged_df$ISO3)
  
  # Obtenir le code ISO3 actuel
  current_country <- reactive({
    country_codes[country_index()]
  })
  
  # Obtenir le nom du pays actuel
  current_country_name <- reactive({
    merged_df %>% filter(ISO3 == current_country()) %>% pull(Country) %>% unique()
  })
  
  # Mettre à jour le graphique
  output$tempDenguePlot <- renderPlot({
    plot_temp_dengue(current_country())
  })
  
  # Mettre à jour le nom du pays
  output$country_name <- renderText({
    paste("Pays actuel :", current_country_name())
  })
  
  # Logique pour passer au pays précédent
  observeEvent(input$btn_prev, {
    if (country_index() > 1) {
      country_index(country_index() - 1)
    }
  })
  
  # Logique pour passer au pays suivant
  observeEvent(input$btn_next, {
    if (country_index() < length(country_codes)) {
      country_index(country_index() + 1)
    }
  })
}

# Exécuter l'application
shinyApp(ui = ui, server = server)


