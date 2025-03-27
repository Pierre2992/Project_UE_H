library(leaflet)
library(dplyr)
library(ggplot2)
library(leaflet.extras)
library(shiny)
library(shinyWidgets)
library(readr)
library(sf)
library(geojsonsf)

# Chargement des données
dengue_data <- read_csv("dengue_data.csv")

# Transformation des données dengue
dengue_data <- dengue_data %>%
  filter(!is.na(dengue_total)) %>%
  group_by(Year, adm_0_name) %>%
  summarise(total_cases = sum(dengue_total, na.rm = TRUE), .groups = 'drop')

# Chargement des données géospatiales des pays
world_map <- sf::st_read("https://github.com/nvkelso/natural-earth-vector/raw/master/geojson/ne_110m_admin_0_countries.geojson")

# Assurer la correspondance des noms de pays
dengue_data <- dengue_data %>%
  rename(NAME = adm_0_name) %>%
  mutate(NAME = toupper(NAME))  # Convertir les noms de pays en majuscules pour une correspondance exacte

world_map <- world_map %>%
  mutate(NAME = toupper(NAME))  # Convertir les noms de pays en majuscules pour une correspondance exacte

# Application Shiny
ui <- fluidPage(
  titlePanel("Carte Interactive des Cas de Dengue"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Année",
                  min = min(dengue_data$Year, na.rm = TRUE),
                  max = max(dengue_data$Year, na.rm = TRUE),
                  value = min(dengue_data$Year, na.rm = TRUE),
                  step = 1,
                  sep = ""),
      selectInput("palette", "Palette de couleurs",
                  choices = c("YlOrRd", "Blues", "Greens", "Purples"),
                  selected = "YlOrRd")
    ),
    mainPanel(
      leafletOutput("map", height = 800)  # Augmenter la hauteur de la carte pour un affichage plus grand
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    data_filtered <- dengue_data %>% filter(Year == input$year)
    
    world_map_filtered <- world_map %>%
      left_join(data_filtered, by = "NAME")
    
    # Vérifier si des données sont disponibles
    if (all(is.na(world_map_filtered$total_cases))) {
      world_map_filtered$total_cases <- 0
    }
    
    pal <- colorNumeric(input$palette, domain = world_map_filtered$total_cases, na.color = "transparent")
    
    leaflet(world_map_filtered) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(ifelse(is.na(total_cases), 0, total_cases)),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste("Pays:", NAME, "<br>Cas de dengue:", ifelse(is.na(total_cases), 0, total_cases))
      ) %>%
      addLegend(
        pal = pal,
        values = ~total_cases,
        title = "Nombre de cas de dengue",
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)