library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(rsconnect)
library(tidyverse)

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

ui <- dashboardPage(
  dashboardHeader(title = "Endangered"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "page1", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "page1",
              column(
                width = 2,
                p("Choose Conservation status"),
                tabPanel(
                  "f1",
                  checkboxGroupInput("f", "Conservation Status", filters[filters$f1=='Conservation Status',]$cate2
                                                                  )
                  
                )
              ),
              column(
                width=10,
                h1("Data Table"),
                
                height=500,
                tabPanel(
                  "mytable",
                  dataTableOutput("myTable")
                )
              )
              
      )
    )
  )
)


server <- function(input, output, session) {
  parks<-read_csv("parks.csv")
  species<-read_csv("species_clean.csv")
  species = subset(species, select = -c(1,2))
  
  data<-full_join(parks,species,by="Park Name")
  
  
  
  

  
  output$myTable = renderDataTable({
    
    return(datatable(data, options = list(scrollX = TRUE),rownames= FALSE))
    
  })
  
  
}

shinyApp(ui = ui, server = server)
