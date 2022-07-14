library(shiny)
library(tidyverse)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "National Park Biodiversity"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram", tabName = "page2", icon = icon("area-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "page2",
              selectInput("box", "Latitude or Longitude:", choices = c("Latitude", "Longitude", "Both")),
              sliderInput("Latitude", "Latitude:", min = 15, max = 70, value = c(15,70), 
                          step = 5), 
              sliderInput("Longitude", "Longitude:", min = -160, max = -65, value = c(-160, -65), 
                          step = 5),
              plotOutput("plot1")
      )
    )
  )
)

server <- function(input, output, session) {
  species <- read.csv("species_clean.csv")
  parks <- read.csv("parks.csv")

  data111 <-
    species %>%
    left_join(parks, by="Park.Name")
  
  output$plot1 = renderPlot({
    lo <- 
      data111 %>% 
      filter(Nativeness=="Native") %>%
      ggplot() +
      geom_histogram(mapping = aes(x = Longitude), fill="deepskyblue", color = "black") + 
      xlim(input$Longitude)
    
    la <- 
      data111 %>% 
      filter(Nativeness=="Native") %>%
      ggplot() +
      geom_histogram(mapping = aes(x = Latitude), fill="#E69F00", color = "black") + 
      xlim(input$Latitude)
    
    f <-
    data111 %>%
      group_by(Park.Name) %>%
      summarise(species_count=n()) %>%
      left_join(parks, by="Park.Name") %>%
      ggplot() +
      geom_point(mapping = aes(x = Longitude, y=Latitude, size=species_count), color = "#E69F00") +
      xlim(input$Longitude) + 
      ylim(input$Latitude)
    
    if(input$box == "Latitude") {
      la
    } else if(input$box == "Longitude") {
      lo
    } else {f}
  })
}

shinyApp(ui = ui, server = server)


# species <- read.csv("species_clean.csv")
# parks <- read.csv("parks.csv")
# 
# data <- 
#   species %>% 
#   left_join(parks, by="Park.Name")
# 
# #Dot plot
# f <- 
# data %>% 
#   group_by(Park.Name) %>% 
#   summarise(species_count=n()) %>% 
#   left_join(parks, by="Park.Name") %>% 
#   ggplot() + 
#   geom_point(mapping = aes(x = Longitude, y=Latitude, size=species_count), color = "#E69F00")
# 
# 
# ggplotly(f)
# 
# # Histogram
# data %>% 
#   filter(Nativeness=="Native") %>% 
#   ggplot() +
#   geom_histogram(mapping = aes(x = Latitude), fill="#E69F00", color = "black")
