
# Pennsylvania Buildings & Parks
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
library(dplyr)
library(plotly)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(directlabels)
library(geomtextpath)
library(ggrepel)
library(treemap)
library(treemapify)
library(ggplotify)
library(formattable)
library(DT)



# Data on Buildings in Pennsylvania (https://www.sciencebase.gov/catalog/item/4f70b240e4b058caae3f8e1b)
pa_data <- st_read("STRUCT_Pennsylvania_State_Shape/Shape/Struct_Point.shp")

#pa_data <- st_transform(pa_data, CRS("+proj=gnom +lat_0=90 +lon_0=-50"))

sf::st_transform(pa_data, 4326)


#change building type to categorical variable
pa_data <- pa_data %>%
  mutate(FType = as.factor(FType))

#assign labels to building type using the building name
pa_data <- pa_data %>%
  mutate(FType = recode(FType, '730' = 'School', '740' = 'Police Department', '780' = 'Post Office',
                        '800' = 'Hospital', '820' = 'Cemetery', '830' = 'Municipal Building'))


# Data on Parks in Pennsylvania (https://www.pasda.psu.edu/uci/DataSummary.aspx?dataset=114)
pa_parks <- st_read("PA_Parks/DCNR_StateParks201703.shp")

#pa_parks <- st_transform(pa_parks, CRS("+proj=gnom +lat_0=90 +lon_0=-50"))

pa_parks <- sf::st_transform(pa_parks, 4326)

#drop nulls
pa_parks <- slice(pa_parks, 1:(n() - 1))

#change park type to categorical variable
pa_parks <- pa_parks %>%
  mutate(TYPE = as.factor(TYPE))


# UI
ui <- navbarPage("Pennsylvania Buildings & Parks",
                 theme = shinytheme("cosmo"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              
                              fluidRow(
                                tabBox(title = "Plots",
                                       width = 12,
                                       tabPanel("Buildings", plotlyOutput("plot1")),
                                       tabPanel("Parks", plotlyOutput("plot2"))
                                       )),
                              
          
                              
                              # Select City
                              selectInput(inputId = "cityinput",
                                          label = "Select a city/town in Pennsylvania",
                                          choices = sort(unique(pa_data$City)),
                                          selected = "Pittsburgh",
                                          multiple = F),
                              

                              #Select Building Type
                              checkboxGroupInput(inputId = "buildinginput",
                                          label = "Select Building Type",
                                          choices = c("School", "Police Department", "Post Office", "Hospital", "Cemetery", "Municipal Building"),
                                          selected = c("School")),


                              
                              # Select Park Type
                              checkboxGroupInput(inputId = "parkinput",
                                          label = "Select Park Type",
                                          choices = c("STATE PARK", "ENVIRONMENTAL EDUCATION CENTER", "CONSERVATION AREA", "FARM PARK", "PRESERVE"),
                                          selected = c("STATE PARK", "ENVIRONMENTAL EDUCATION CENTER", "CONSERVATION AREA", "FARM PARK", "PRESERVE"))

                            ),

            
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}"),
                                         
                              # Map Output
                              leafletOutput("map")
                              
                        
                            )
                          ),
                 ),
                 
                 
                 
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                            column(6, "Buildings Data",
                            fluidRow(
                              
                              # Download Button for Buildings data
                              downloadLink('downloadData1', 'Download Buildings Data'),
                              
                              # Display Buildings data
                              wellPanel(DT::dataTableOutput("table1")),
                            
                            
                          )
                          ),
                          column(width = 6, "Parks Data",
                                 fluidRow(
                                   
                                   # Download Button for Parks data
                                   downloadLink('downloadData2', 'Download Parks Data'),
                                   
                                   # Display Parks data
                                   wellPanel(DT::dataTableOutput("table2")),
                                   
                                   
                                      )
                                 )
                          )
                 )
)

# Server
server <- function(input, output, session) {
  
  # Filtered Data for Buildings
  data1 <- reactive({
  
    #requirements for inputs mandatory
    req(input$cityinput)
    req(input$buildinginput)
    
    #subset of buildings data based on user input on city and building type
    buildings <- pa_data %>%
      filter(City %in% input$cityinput) %>%
      filter(FType %in% input$buildinginput)
    
    #dataset has to have non-zero rows otherwise it'll crash
    if (nrow(buildings) > 0) {
      
      buildings <- buildings
    }

    return(buildings)
    
  })


  # Filtered Data for Parks
  data2 <- reactive({
  
    #requirement for input mandatory
    req(input$parkinput)
    
    #subset of parks data based on user input on park type
    parks <- subset(pa_parks, TYPE %in% input$parkinput)
    
    return(parks)
  })
  
  
  #Plot Map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles(group = "Open Street Map") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap","Esri"),
        overlayGroups = c("Buildings", "Parks"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      setView(-77, 41, 7) #view for Pennsylvania

  })
  
  # alternative for map layers
  #
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles(group = "Open Street Map") %>%
  #     addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri") %>%
  #     addLayersControl(
  #       baseGroups = c("Open Street Map","Esri")) %>%
  #     setView(-77, 41, 7) 
  #   
  # })
  # 
    
  #create an observer event: if no input for building type then remove markers on map for buildings
  observeEvent(length(input$buildinginput) == 0, {
    leafletProxy("map") %>%
      clearGroup(group = "Buildings") 
  })
  
  #create an observer event: change building type based on city input (this is not working - tried many times)
  # observeEvent(
  #   input$cityinput, {
  #     updateCheckboxGroupInput(session, "buildinginput",
  #                       label = "Select Building Type",
  #                       choices = data1()$FType, 
  #                       selected = input$buildinginput)
  #   })
  
    #change building map based on user inputs
    observe({
      req(input$cityinput)
      req(input$buildinginput)
      
      leafletProxy("map") %>%
        clearGroup(group = "Buildings") %>%
        addCircleMarkers(data = data1(), stroke = TRUE, label = ~Name, group = "Buildings",
                         labelOptions = labelOptions(noHide = FALSE, offset=c(0,-12), textOnly = TRUE, style=list("color" = "black",
                                                                                                                  "font-style" = "bold",
                                                                                                                  "font-size" = "16px")))
      
    })
    
    #change parks map based on user input
    observe({
      req(input$cityinput)
      req(input$buildinginput)
      
      leafletProxy("map") %>%
        clearGroup(group = "Parks") %>%
        addPolygons(data = data2(),  label = ~PARK_NAME, group = "Parks", color = "darkgreen", weight = 2, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = "darkgreen", labelOptions = labelOptions(noHide = FALSE, offset=c(0,-12), textOnly = TRUE, style=list("color" = "black",
                                                                                                                                      "font-style" = "bold",
                                                                                                                                      "font-size" = "16px")))
      
    })
    
  
   
  #plots
    
  #buildings plot
  output$plot1 <- renderPlotly({
    
    dataplot1 <- data1() %>% 
      group_by(FType) %>%
      summarise(total_buildings = n())
    
    plot1 <- ggplot(data = dataplot1, aes(x = FType, y = total_buildings, fill = FType,
                                          text = paste("Type:",FType))) +
      geom_bar(stat = 'identity') + theme_classic() + labs(x = "Building Type",
                                                           y = "Number of Buildings",
                                                           title = "Buildings in Pennsylvania") +
      theme(legend.position = "none")
    
    plot1 <- plot1 %>% 
      ggplotly(tooltip="text")
    
    
  })
  
  #parks plot
  output$plot2 <- renderPlotly({
    
    dataplot2 <- as_tibble(data2())
    
    dataplot2 <- dataplot2 %>%
      select('PARK_NAME', 'TYPE')
    
    dataplot2 <- dataplot2 %>%
      group_by(TYPE) %>%
      summarise(total_parks = n())
    
    plot_ly(dataplot2, labels = ~TYPE, values = ~total_parks) %>% add_pie(hole = 0.6) %>%
      layout(title = 'Parks in Pittsburgh')
    
  })
  
  
  
  # #data table for buildings
  output$table1 <- DT::renderDataTable(data1(), options = list(scrollX = T))
  
  #download data for buildings
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste('buildings', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data1(), con)
    }
  )
  
  # #data table for parks
  output$table2 <- DT::renderDataTable(data2(), options = list(scrollX = T))

  #download data for parks
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste('parks', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data2(), con)
    }
  )

 
}

# Run the application 
shinyApp(ui = ui, server = server)

