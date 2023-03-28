library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(lubridate)

#pollution data (https://data.world/data-society/us-air-pollution-data)
data_cleaned <- read.csv("pollution_data_cleaned.csv")

pollution_data <- data_cleaned %>%
  group_by(State, County, Year) %>%
  summarise_at(vars(NO2.AQI, O3.AQI, SO2.AQI, CO.AQI), list(mean = mean))


state_data <- data_cleaned %>%
  group_by(State, Year) %>%
  summarise_at(vars(NO2.AQI, O3.AQI, SO2.AQI, CO.AQI), list(mean = mean))

state_names <- unique(pollution_data$State)
county_names <- unique(pollution_data$County)


#map data
us_states <- map_data("state")
us_counties <- map_data("county")

us_counties$subregion = str_to_title(us_counties$subregion)

# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Theme selector --------------------------------------------------
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("lumen"),
  
  # Application title -----------------------------------------------
  titlePanel("Air Pollution in United States (2000 - 2016)"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      radioButtons(inputId = "y", 
                  label = "Pollutant:",
                  choices = c('NO2.AQI_mean', 'O3.AQI_mean', 'SO2.AQI_mean', 'CO.AQI_mean'), 
                  selected = "NO2.AQI_mean"),
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Download Button
      downloadLink('downloadData', 'Download Data'),

  ),
      
      
  # Output: -------------------------------------------------------
  mainPanel(
    
    # Show plot1 --------------------------------------------
    h4("AQI Level Over Time", align = "left"),
    br(),
    
    # Select which State to plot ---------------------------
    selectInput(inputId = "selected_type1",
                label = "Select State:",
                choices = state_names,
                selected = "Pennsylvania"),
    
    # Select which Counties to plot ------------------------
    selectInput(inputId = "selected_type2",
                label = "Select County:",
                choices = county_names,
                selected = "Allegheny"),
    
    
    plotOutput(outputId = "plot1"),
    br(),
    
    # Show plot2 -------------------------------------------
    h4("AQI Level Across U.S. Counties Over Time", align = "left"),
    br(),
    sliderInput(inputId = "yearinput", 
                label = "Year", 
                min = 2000, max = 2016, step = 1, 
                value = 2000),
    
    
    plotOutput(outputId = "plot2"),
    br(),

    # Show plot3 --------------------------------------------
    h4("AQI Level Across U.S. States Over Time", align = "left"),
    br(),
    plotOutput(outputId = "plot3"),
    br(),

    
    # Show data table ---------------------------------------------
    h4("Data Table", align = "left"),
    br(),
    DT::dataTableOutput(outputId = "datatable")
    )
  )
)
  
# Define server function required to create the scatterplot ---------
server <- function(input, output, session) {
  
  
  # Create a subset of data filtering for selected title types ------
  data_subset <- reactive({
    filter(pollution_data, State %in% input$selected_type1 & County %in% input$selected_type2)
  })
  

  plot_data <- reactive({
      a <- subset(data_subset(), County == input$selected_type2, 
                  select=c(State:CO.AQI_mean))
      return(a)
      
    })
  
  # map_data <- reactive({
  #   county_data <- subset(pollution_data, Year %in% input$yearinput, c(County:CO.AQI_mean))
  #   county_data_map <- left_join(us_counties, county_data, by=c("subregion"="County"))
  #   return(county_data_map)
  # })
  
  # barchart_data <- reactive({
  #   barchart_data <- state_data
  #   return(barchart_data)
  # })
  
  #line chart
  output$plot1 <- renderPlot({
    ggplot(plot_data(), aes_string(x = plot_data()$Year, y = input$y)) + 
      geom_line(color = "deepskyblue3", size = 2) +
      labs(x = "Year",
           y = toTitleCase(str_replace_all(input$y, "_", " ")))
    })
  
  #county map
  # output$plot2 <- renderPlot({
  #   map_data() %>%
  #     ggplot(aes_string(x=map_data()$long,y=map_data()$lat,group=map_data()$group, fill = input$y)) +
  #     geom_polygon(color = "gray90", size = 0.1) +
  #     coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  #     scale_fill_continuous(name = toTitleCase(str_replace_all(input$y, "_", " "))) +
  #     theme(legend.position="bottom",
  #           axis.line=element_blank(),
  #           axis.text=element_blank(),
  #           axis.ticks=element_blank(),
  #           axis.title=element_blank(),
  #           panel.background=element_blank(),
  #           panel.border=element_blank(),
  #           panel.grid=element_blank())
  #   
  #   
  # })
  
  #bar chart
  output$plot3 <- renderPlot({
    ggplot(barchart_data(), aes_string(x=barchart_data()$Year, y=input$y)) + 
      geom_bar(stat = "identity", fill = "deepskyblue3") + coord_flip() + facet_wrap(~barchart_data()$State, ncol = 6) +
      labs(x = '', y = toTitleCase(str_replace_all(input$y, "_", " ")))
  })
  

    
  # Print data table if checked -------------------------------------
  output$datatable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = data_subset()[, 1:7], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  # Download Button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data_subset', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data, con)
    }
  )
  
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
  