library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(ggplot2)



ui <- dashboardPage(
    dashboardHeader(title = "BPD Arrest"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Plotly", tabName = "page1", icon = icon("line-chart")),
            menuItem("Density", tabName = "page2", icon = icon("area-chart")),
            menuItem("Map", tabName = "page3", icon = icon("map-o")),
            menuItem("Data", tabName = "page4", icon = icon("fa-solid fa-database"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    checkboxInput("holiday", label = "Show holidays", value = FALSE),
                    plotlyOutput("plot2", height = 500)
                    ),
            tabItem(tabName = "page2",
                    sliderInput("year", "Year:", min = 2014, max = 2020, value = 1, 
                                step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                    plotOutput("plot1")
                    ),
            tabItem(tabName = "page3",
                    leafletOutput("myMap", width="100%")
                    ),
            tabItem(tabName = "page4",
                    dataTableOutput("myTable")
            )
        )
    )
)


server <- function(input, output, session) {
    ## ----------------------------------------------- ##
    # read data.csv
    data = read.csv("data.csv")
    data$ArrestDate = as.Date(data$ArrestDate, '%m/%d/%Y')
    class(data$ArrestDate)
    
    data$Longitude = round(data$Longitude, digits = 5)
    data$Latitude = round(data$Latitude, digits = 5)
    
    # read usholidays.csv
    holidays = read.csv("usholidays.csv")
    holidays$Date = as.Date(holidays$Date, '%Y-%m-%d')
    words = unique(holidays$Holiday)
    Abb = c("NYD","MLKB","WaB", "MeD", "InD", "LaD", "CoD", "VeD", "ThD", "ChD",
          "NYD","MLKB","WaB")
    holidays$Abb = holidays$Holiday
    for (i in 1:length(words)) {
      holidays$Abb=str_replace(holidays$Abb,words[i],Abb[i])
    }
    
    CrimeCountByDay = summarise(group_by(data, ArrestDate), ObsCount = n())
    CrimeCountByDay$Date = CrimeCountByDay$ArrestDate
    CrimeCountByDay$ArrestDate = NULL
    
    ## -------------------------------------------  ##
    # graphs start here
    output$plot1 = renderPlot({
      densityplot = data %>%
        filter(as.numeric(format(ArrestDate, '%Y')) == input$year) %>%
        ggplot(aes(x = Age, color = Sex)) +
        geom_density(alpha = 0) +
        labs(title = 'Density Plot',
             subtitle = 'Age&Sex',
             x = 'Age',
             y = 'Density',
             ylim = c(0, 1),
             xlim = c(0, 100)) +
        annotate('text', x = 70, y = 0.03, label = input$year) + 
        scale_color_discrete(labels = c('F', 'M')) + 
        theme(axis.text.y=element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank())
      return(densityplot)
        
    })
    
    output$plot2 = renderPlotly({
      
      data_hol = full_join(CrimeCountByDay, holidays)
      class(data_hol$Date)
      
      data_hol=filter(data_hol, Date >= as.Date("2014-01-01"))
      data_hol=filter(data_hol, Date <= as.Date("2020-05-01"))
      
      
      f = data_hol%>%
        ggplot(aes(Date, ObsCount)) +
        geom_line() +
        geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))+
        labs(title = "Arrests in Baltimore",
             x = "Date",
             y = "Number of Arrests")
      data_subset = data_hol %>% filter(!is.na(Holiday))
      if(input$holiday==TRUE){
        data_subset = data_hol %>% filter(!is.na(Holiday))
        f = f + geom_point(data = data_subset, color="purple")+
          geom_text(data = data_subset, aes(x=Date, y=ObsCount, label=Abb))
      }
      f = ggplotly(f)
      
      return(f)
        
    })
    
    output$myMap = renderLeaflet({
      loc_data= data %>%
        group_by(lng=round(Longitude,3),lat=round(Latitude,3)) %>%
        summarise(N=n())
      
      loc_data= loc_data%>%
        mutate(latL = lat-0.0005)%>%
        mutate(latH = lat+0.0005)%>%
        mutate(lngL = lng-0.0005)%>%
        mutate(lngH = lng+0.0005)
      
      m=loc_data %>% leaflet() %>% addTiles() %>%
        setView(-76.6,39.31, zoom=12) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
        addLayersControl(baseGroups = c("Toner", "OSM"),options = layersControlOptions(collapsed = FALSE))%>%
        addRectangles(
          lng1=~lngL, lat1=~latL,
          lng2=~lngH, lat2=~latH,
          fillOpacity = ~N/150, opacity = 0, fillColor = "red", label = ~N)
      
      return(m)
    })
    
    output$myTable = renderDataTable({
      datatable=datatable(data)
      return(datatable(data, rownames= FALSE))
    })
    
}

shinyApp(ui = ui, server = server)
