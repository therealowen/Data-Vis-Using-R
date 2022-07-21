library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggrepel)
library(scales)
library(ggthemes)

filters<-
  data.frame(
    f1 = c(
      'Conservation Status',
      'Conservation Status',
      'Conservation Status',
      'Conservation Status',
      'Conservation Status',
      'Conservation Status',
      'Conservation Status',
      'Conservation Status',
      'Conservation Status',
      'Conservation Status',
      'Conservation Status',
      'Conservation Status'
      
    ),
    cate2 = c(
      "Least Concerned",     
      "Species of Concern", 
      "Endangered",          
      "In Recovery",        
      "Threatened",          
      "Under Review",       
      "Proposed Threatened", 
      "Extinct",            
      "Proposed Endangered", 
      "Resident",           
      "Breeder",             
      "Migratory"
    )
    
  )

park_name <- c("Acadia National Park",                          
                           "Arches National Park",                         
                           "Badlands National Park",                        
                           "Big Bend National Park",                        
                           "Biscayne National Park",                        
                           "Black Canyon of the Gunnison National Park",    
                           "Bryce Canyon National Park",                    
                           "Canyonlands National Park",                     
                           "Capitol Reef National Park",                    
                           "Carlsbad Caverns National Park",                
                           "Channel Islands National Park",                 
                           "Congaree National Park",                        
                           "Crater Lake National Park",                     
                           "Cuyahoga Valley National Park",                 
                           "Denali National Park and Preserve",             
                           "Death Valley National Park",                    
                           "Dry Tortugas National Park",                    
                           "Everglades National Park",                      
                           "Gates Of The Arctic National Park and Preserve",
                           "Glacier National Park",                       
                           "Glacier Bay National Park and Preserve",        
                           "Great Basin National Park",                     
                           "Grand Canyon National Park",                    
                           "Great Sand Dunes National Park and Preserve",   
                           "Great Smoky Mountains National Park",           
                           "Grand Teton National Park",                     
                           "Guadalupe Mountains National Park",             
                           "Haleakala National Park",                       
                           "Hawaii Volcanoes National Park",                
                           "Hot Springs National Park",                     
                           "Isle Royale National Park",                     
                           "Joshua Tree National Park",                     
                           "Katmai National Park and Preserve",             
                           "Kenai Fjords National Park",                   
                           "Kobuk Valley National Park",                    
                           "Lake Clark National Park and Preserve",        
                           "Lassen Volcanic National Park",                 
                           "Mammoth Cave National Park",                    
                           "Mesa Verde National Park",                      
                           "Mount Rainier National Park",                   
                           "North Cascades National Park",                  
                           "Olympic National Park",                        
                           "Petrified Forest National Park",                
                           "Pinnacles National Park",                       
                           "Redwood National Park",                        
                           "Rocky Mountain National Park",                  
                           "Saguaro National Park",                         
                           "Sequoia and Kings Canyon National Parks",       
                           "Shenandoah National Park",                      
                           "Theodore Roosevelt National Park",              
                           "Voyageurs National Park",                       
                           "Wind Cave National Park",                       
                           "Wrangell - St Elias National Park and Preserve",
                           "Yellowstone National Park",                     
                           "Yosemite National Park",                        
                           "Zion National Park")

ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "National Park Biodiversity", titleWidth = 300),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "page1", icon = icon("home")),
            menuItem("Chart", tabName = "Page5", icon = icon("chart-bar")),
            menuItem("Histogram", tabName = "page2", icon = icon("chart-area")),
            menuItem("Map", tabName = "page3", icon = icon("map")),
            menuItem("Data", tabName = "page4", icon = icon("database"))
        )
    ),
    
    dashboardBody(
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }",
        "body { font-size: 16px; }"
      ),
    
      tabItems(
          tabItem(
            tabName = "page1",
            h2(strong("Introduction")), br(),
            fluidRow(
              box(
                title = "About This Dataset",
                solidHeader = TRUE,
                status = "warning",
                width = 12,
                collapsible = TRUE,
                column(12,
                       tags$div(fluidRow(
                         column(
                           9,
                           h4(
                             strong("The National Park"),
                             " Service publishes a database of animal and plant species identified in individual 
                             national parks and verified by evidence — observations, vouchers, or reports that document the p
                             resence of a species in a park. All park species records are available to the public on the National 
                             Park Species portal; exceptions are made for sensitive, threatened, or endangered species when 
                             widespread distribution of information could pose a risk to the species in the park.",
                              ),
                           a(h5("Source: Kaggle"), href =
                               'https://www.kaggle.com/datasets/nationalparkservice/park-biodiversity')
                         ),
                         column(3, tags$img(src = "logo.png", width = "100%"))
                       )),)
              )
            ),
            fluidRow(
              box(
                title = "About the Application",
                solidHeader = TRUE,
                status = "success",
                width = 12,
                collapsible = TRUE,
                column(
                  9,
                  h4(
                    "This website titled",
                    strong("National Park Biodiversity"),
                    "aims to provide data visualization and statistics for 
                    providing information on the presence and status of species from park to park."
                   )
                ),
                column(3, tags$img(src = "np2.jpg", width = "100%"))
              )
            ),
            
            fluidRow(
              box(
                title = "About Us",
                solidHeader = TRUE,
                status = "info",
                width = 12,
                collapsible = TRUE,
                column(12,
                       tags$div(fluidRow(
                         column(
                           9,
                           h4(
                             "We developed this website for the completion of Data Visualization
                             course final project during our MS Information Systems degree at 
                             Johns Hopkins Carey Business School.", 
                             br(),
                             br(),
                             strong("Team Member: "), "Tanush Sharanarthi, Shreyas Iyer, 
                             Owen Hu, Steven Lu, Junhua Zhu"
                           )
                         ),
                         column(3, tags$img(src = "carey.png", width = "100%"))
                       )),),
              )
            ),
            
            fluidRow(
              box(
                title = "References",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                column(12,
                       tags$div(fluidRow(
                         column(
                           9,
                           h4(
                             "1. https://www.kaggle.com/datasets/nationalparkservice/park-biodiversity", 
                             br(),
                           )
                         ),
                         column(3, tags$img(src = "references.png", width = "100%"))
                       )),),
              )
            )
          ),
  
          tabItem(tabName = "Page5",
                  fluidRow(
                    column(4,img(src='india-photos-kanha-national-park.jpeg', align = "center",height='300px')),
                    column(4,img(src='nationalpark-yosemitetopimage-2x1-lowres.webp', align = "center",height='300px')),
                    column(4,img(src='6ORbWENY-1380x690.jpeg', align = "center",height='300px'))
                  ),
                  h2(strong("Chart for national park species")),
                  br(),
                  valueBox(
                    value = textOutput("text1"),
                    subtitle = "percentage of endangered species",
                    color = "purple",
                    icon = icon("pie-chart"),
                    width = 4),
                  valueBox(
                    value = textOutput('text2'),
                    subtitle = "number of species in the park",
                    color = "blue",
                    icon = icon("qq"),
                    width = 4),
                  valueBox(
                    value = textOutput('text3'),
                    subtitle = "number of rare species in the park",
                    color = "yellow",
                    icon = icon("book"),
                    width = 4),
                  br(),
                  fluidRow(
                    box(
                      width =12,
                      selectInput(inputId = "park", label = h3("Select a park"), 
                                  choices = park_name, 
                                  selected = 1)
                    )
                  ),
                  fluidRow(
                    column(5,plotOutput("plot2")),
                    column(7,plotOutput("plot3"))
                  )
          ),
          
          # part 3
          tabItem(tabName = "page2",
                  selectInput("box", "Latitude or Longitude:", choices = c("Latitude", "Longitude", "Both")),
                  sliderInput("Latitude", "Latitude:", min = 15, max = 70, value = c(15,70), 
                              step = 5), 
                  sliderInput("Longitude", "Longitude:", min = -160, max = -65, value = c(-160, -65), 
                              step = 5),
                  plotOutput("plot1")
          ),
          
          # part 4
          tabItem(tabName = "page3", h2("National Parks Map"),
                  leafletOutput("myMap", width = "100%"), br(), h2("Species found in the region"),
                  dataTableOutput("table")), 
          
          # part 5
          tabItem(tabName = "page4",
                  fluidRow(
                    column(
                      width = 2,
                      p("Choose Conservation status"),
                      tabPanel(
                        "f1",
                        checkboxGroupInput("myFilters", "Conservation Status", filters[filters$f1 ==
                                                                                         'Conservation Status', ]$status)
                        
                      )
                    ),
                  column(
                    width = 10,
                    h1("Data Table"),
                    tabPanel("mytable",
                             dataTableOutput('myTable'))
                  ))
                  
          )
            
            
        )
    )
)


server <- function(input, output, session) {
    ## ----------------------------------------------- ##
    species <- read_csv("species_clean.csv")
    parks <- read_csv("parks.csv")
    
    
    species_number = species %>% 
      group_by(`Park Name`) %>%
      summarise(count = n())
    
    data <-species %>%left_join(parks, by="Park Name") #Steven code
    species_shreyes = subset(species, select = -c(1,2)) # Shreyes code
    data_shreyes<-full_join(parks,species_shreyes,by="Park Name") # Shreyes code
    # -- new -- #
    data_shreyes$status[grep(
      "Least Concerned",
      data_shreyes$`Conservation Status`,
      ignore.case = TRUE,
      fixed = FALSE
    )] <- "Least Concerned"
    
    data_shreyes$status[grep(
      "Species of Concern",
      data_shreyes$`Conservation Status`,
      ignore.case = TRUE,
      fixed = FALSE
    )] <- "Species of Concern"
    
    data_shreyes$status[grep(
      "Endangered",
      data_shreyes$`Conservation Status`,
      ignore.case = TRUE,
      fixed = FALSE
    )] <- "Endangered"
    
    data_shreyes$status[grep(
      "In Recovery",
      data_shreyes$`Conservation Status`,
      ignore.case = TRUE,
      fixed = FALSE
    )] <- "In Recovery"
    
    data_shreyes$status[grep(
      "Threatened",
      data_shreyes$`Conservation Status`,
      ignore.case = TRUE,
      fixed = FALSE
    )] <- "SThreatened"
    
    data_shreyes$status[grep(
      "Under Review",
      data_shreyes$`Conservation Status`,
      ignore.case = TRUE,
      fixed = FALSE
    )] <- "Under Review"
    
    data_shreyes$status[grep(
      "Proposed Threatened",
      data_shreyes$`Conservation Status`,
      ignore.case = TRUE,
      fixed = FALSE
    )] <- "Proposed Threatened"
    
    data_shreyes$status[grep("Extinct",
                             data_shreyes$`Conservation Status`,
                     ignore.case = TRUE,
                     fixed = FALSE)] <- "Extinct"
    
    data_shreyes$status[grep(
      "Proposed Endangered",
      data_shreyes$`Conservation Status`,
      ignore.case = TRUE,
      fixed = FALSE
    )] <- "Proposed Endangered"
    
    data_shreyes$status[grep("Resident",
                             data_shreyes$`Conservation Status`,
                     ignore.case = TRUE,
                     fixed = FALSE)] <- "Resident"
    
    data_shreyes$status[grep("Breeder",
                             data_shreyes$`Conservation Status`,
                     ignore.case = TRUE,
                     fixed = FALSE)] <- "Breeder"
    
    data_shreyes$status[grep(
      "Migratory",
      data_shreyes$`Conservation Status`,
      ignore.case = TRUE,
      fixed = FALSE
    )] <- "Migratory"
    
    data_shreyes_table <- reactive({
      if ((length(input$myFilters)) ==
          0) {
        return(data_shreyes[,1:17])
      } else{
        return(data_shreyes[(data_shreyes$status %in% input$myFilters), 1:17])
      }
    })
    
    # -- new -- #
  
    data2 <- species %>% left_join(parks, by="Park Name") # steven code 
    ## -------------------------------------------  ##
    # graphs start here
    # 1. histogram
    output$plot1 = renderPlot({
      lo <- 
        data %>% 
        filter(Nativeness=="Native") %>%
        ggplot() +
        geom_histogram(mapping = aes(x = Longitude), fill="skyblue", color = "black") + 
        xlim(input$Longitude) + 
        ggtitle("Number of species for different Longitude") +
        ylab("Number of species")
      
      la <- 
        data %>% 
        filter(Nativeness=="Native") %>%
        ggplot() +
        geom_histogram(mapping = aes(x = Latitude), fill="#E69F00", color = "black") + 
        xlim(input$Latitude) + 
        ggtitle("Number of species for different Latitude") +
        ylab("Number of species")
      
      f <-
        data %>%
        group_by(`Park Name`) %>%
        summarise(species_count=n()) %>%
        left_join(parks, by="Park Name") %>%
        ggplot() +
        geom_point(mapping = aes(x = Longitude, y=Latitude, size=species_count), color = "#E69F00") +
        xlim(input$Longitude) + 
        ylim(input$Latitude) + 
        ggtitle("Number of species for different Longitude and Latitude")
      
      if(input$box == "Latitude") {
        la
      } else if(input$box == "Longitude") {
        lo
      } else {f}
    })
    
    # 2. map 
    national_park_icon = makeIcon("logo.png", iconWidth = 30, iconHeight = 35)
    
    output$myMap = renderLeaflet({
      parks %>%
        leaflet() %>%
        addTiles() %>%
        setView(-100.72, 48.75, zoom = 3) %>%
        addProviderTiles(providers$Stamen.TerrainBackground, group = "TerrainBackground") %>%
        addLayersControl(
          baseGroups = c("TerrainBackground", "OSM"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addMarkers(
          lng = ~ Longitude,
          lat = ~ Latitude,
          clusterOptions = markerClusterOptions(),
          icon = national_park_icon,
          popup = ~ paste0("<b>",
                           `Park Name`,"</b>",
                           br(),
                           "Number of species: ",
                           species_number$count[species_number$`Park Name` == `Park Name`]
          )
        )
    })
    
    output$species_region <-
      renderText('No National Park selected on map')
    
    observeEvent(input$myMap_marker_click, {
      print("observed button_click and get id from map_marker_click$id")
      lat <- input$myMap_marker_click$lat
      lon <- input$myMap_marker_click$lng
      print(lat)
      print(lon)
      output$table <-
        renderDataTable(return (datatable(
          data2[data2$Latitude == lat &
                  data2$Longitude == lon, 2:14],
          rownames = FALSE,
          options = list(scrollX = TRUE)
        )))
      output$species_region <-
        renderText(paste0('Species found in ', data2[data2$Latitude == lat &
                                                       data2$Longitude == lon, ]$`Park Name`[[1]][1]))
    })
    
    
    # 3. data table
    output$myTable = renderDataTable(data_shreyes_table(), options = list(scrollX = TRUE))
    #   renderDataTable({
    #   
    #   return(datatable(data_shreyes_table, options = list(scrollX = TRUE),rownames= FALSE))
    #   
    # })
    
    
    
    #4 pie chart + bar chart + valuebox
    #text
    #percentage of endangered species
    output$text1 <- renderText({ 
      species_bar = species %>%
        filter(species$`Park Name` == input$park) %>%
        group_by(`Conservation Status`) %>%
        summarise(count  = n())  
      species_endangered <- species_bar %>% filter(`Conservation Status` == 'Endangered')
      return (paste( round(species_endangered$count / sum(species_bar$count),4)*1000,'%'))
    })
    
    #most common seasonality 
    output$text2 <- renderText({ 
      species_park <- species %>%
        filter(species$`Park Name` == input$park)
      return (nrow(species_park))
    })
    
    #rare species
    output$text3 <- renderText({ 
      species_rare <- species %>%
        filter(`Park Name` == input$park) %>%
        filter(Abundance == 'Rare')
      return (nrow(species_rare))
    })
    
    #plot
    output$plot3 <- renderPlot({
      bar_chart <- species %>%
        filter(species$`Park Name` == input$park) %>%
        group_by(`Conservation Status`) %>%
        summarise(count  = n()) %>%
        ggplot(mapping = (aes(x = count,y = `Conservation Status`,fill = `Conservation Status`))) +
        geom_bar(stat="identity") + 
        labs(title = paste("bar chart for",'conservation status'),
             subtitle = input$park) +
        ylab('species status')
      return(bar_chart)
    })
    
    output$plot2 <- renderPlot({
      species_bar = species %>%
        filter(species$`Park Name` == input$park) %>%
        group_by(`Conservation Status`) %>%
        summarise(count  = n())  
      ggplot(species_bar, aes(x="", y=count, fill=`Conservation Status`)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        labs(title = 'pie chart for conservation status',subtitle = input$park) +
        scale_fill_discrete(name=NULL)+
        ylab('') +xlab('')
    })
}

shinyApp(ui = ui, server = server)
