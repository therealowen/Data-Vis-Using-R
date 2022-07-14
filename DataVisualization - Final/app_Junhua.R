library(tidyverse)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(leaflet)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "National park species"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("chart", tabName = "Page3", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Page3",
              shinyDashboardThemes(
                theme = "blue_gradient"
              ),
              fluidRow(column(4,uiOutput("img2")),
                       column(4,uiOutput("img1")),
                       column(4,uiOutput("img3"))
                       ),
              h2("Chart for national park species"),
              br(),
              valueBox(
                value = textOutput("text1"),
                subtitle = "percentage of endangered species",
                color = "purple",
                icon = icon("chart-pie"),
                  width = 4),
              valueBox(
                value = textOutput('text2'),
                subtitle = "number of species",
                color = "blue",
                icon = icon("qq"),
                width = 4),
              valueBox(
                value = '2%',
                subtitle = "percentage of endangered species",
                color = "yellow",
                icon = icon("book"),
                width = 4),
              br(),
              fluidRow(
                box(
                  width =5,
                  selectInput(inputId = "park", label = h3("Select a park"), 
                              choices = all_menu, 
                              selected = 1)
                )
              ),
              fluidRow(
                column(5,plotOutput("plot2")),
                column(7,plotOutput("plot1"))
              )
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #data
  species_clean <- read_csv("species_clean.csv")
  species_clean = data.frame(species_clean)
  all_menu =  species_clean %>%  # all parks name in a list
    distinct(Park.Name) %>%
    pull(Park.Name)  
  
  #text
  #percentage of endangered species
  output$text1 <- renderText({ 
    species_bar = species_clean %>%
      filter(species_clean$Park.Name == input$park) %>%
      group_by(Conservation.Status) %>%
      summarise(count  = n())  
    species_endangered <- species_bar %>% filter(Conservation.Status == 'Endangered')
    return (paste( round(species_endangered$count / sum(species_bar$count),4)*1000,'%'))
  })
  
  #most common seasonality 
  output$text2 <- renderText({ 
    species_park <- species_clean %>%
      filter(species_clean$Park.Name == input$park)
    return (nrow(species_park))
  })

  #plot
  output$plot1 <- renderPlot({
    bar_chart <- species_clean %>%
                  filter(species_clean$Park.Name == input$park) %>%
                  group_by(Conservation.Status) %>%
                  summarise(count  = n()) %>%
    ggplot(mapping = (aes(x = count,y = Conservation.Status,fill = Conservation.Status))) +
      geom_bar(stat="identity") + 
      labs(title = paste("bar chart for",'conservation status'),
           subtitle = input$park) +
      ylab('species status')
    return(bar_chart)
  })
  
  output$plot2 <- renderPlot({
    species_bar = species_clean %>%
      filter(species_clean$Park.Name == input$park) %>%
      group_by(Conservation.Status) %>%
      summarise(count  = n())  
    ggplot(species_bar, aes(x="", y=count, fill=Conservation.Status)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      labs(title = 'pie chart for conservation status',
           subtitle = input$park) +
      ylab('')
  })
  
  #image 
  #max width = 1000 for full screen
  output$img1 <- renderUI({  
    return(img(src = 'india-photos-kanha-national-park.jpeg',
               height = 150) ) })
  output$img2 <- renderUI({
    return(img(src = 'nationalpark-yosemitetopimage-2x1-lowres.webp',
               height = 150) ) }) 
  output$img3 <- renderUI({
    return(img(src = '6ORbWENY-1380x690.jpeg',
               height = 150) ) })
}
  
# Run the application 
shinyApp(ui = ui, server = server)
