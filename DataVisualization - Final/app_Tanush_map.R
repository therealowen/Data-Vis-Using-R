library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(shinyalert)

ui <- dashboardPage(
  dashboardHeader(title = "National Park Biodiversity"),
  dashboardSidebar(sidebarMenu(
    menuItem("Map", tabName = "page1", icon = icon("map-o"))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "page1", h2("National Parks Map"),
            leafletOutput("myMap", width = "100%"), br(), h2("Species found in the region"), dataTableOutput("table"))
  ))
)


server <- function(input, output, session) {
  species = read_csv("species_clean.csv")
  parks = read_csv("parks.csv")
  
  species_number = species %>%
    group_by(`Park Name`) %>%
    summarise(count = n())
  
  output$myMap = renderLeaflet({
    parks %>%
      leaflet() %>%
      addTiles() %>%
      setView(-100.72, 40.75, zoom = 4) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "TerrainBackground") %>%
      addLayersControl(
        baseGroups = c("TerrainBackground", "OSM"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addCircles(
        lng = ~ Longitude,
        lat = ~ Latitude,
        weight = 1,
        radius = 100000,
        popup = ~paste0(
          `Park Name`,
          br(),
          "Number of species:",
          species_number$count[species_number$`Park Name` == `Park Name`],
          actionButton("showmodal", "Expand to show more details",
                       onclick = 'Shiny.onInputChange("button_click",  Math.random())')
        )
      )
    
  })
  
  observeEvent(input$button_click, {
    print("observed button_click and get id from map_marker_click$id")
    id <- input$map_marker_click$id
    output$table <- renderDataTable(return (datatable(species, rownames = FALSE)))
  })
}

shinyApp(ui = ui, server = server)
