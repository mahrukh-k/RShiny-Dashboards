library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(lubridate)

#pollution data
#load data (https://data.world/data-society/us-air-pollution-data)
data <- read.csv("pollution_us_2000_2016.csv")
summary(data)

#pollution levels are recorded multiple times for each date. so we have to drop the duplicates.
data_cleaned <- data %>%
  mutate(Date.Local = ymd(Date.Local))

data_cleaned <- data_cleaned %>%
  mutate(Year = as.numeric(format(Date.Local,'%Y')))

data_cleaned <- data_cleaned[c('State', 'County', 'Year', 'NO2.AQI', 'O3.AQI', 'SO2.AQI', 'CO.AQI')]

data_cleaned <- na.omit(data_cleaned)

pollution_data <- data_cleaned %>%
  group_by(State, County, Year) %>%
  summarise_at(vars(NO2.AQI, O3.AQI, SO2.AQI, CO.AQI), list(mean = mean))


state_data <- data_cleaned %>%
  group_by(State, Year) %>%
  summarise_at(vars(NO2.AQI, O3.AQI, SO2.AQI, CO.AQI), list(mean = mean))

state_names <- unique(pollution_data$State)
county_names <- unique(pollution_data$County)


#map data
county_data <- subset(pollution_data, Year == 2016, c(County:CO.AQI_mean))

us_states <- map_data("state")
us_counties <- map_data("county")

us_counties$subregion = str_to_title(us_counties$subregion)

county_data_map <- left_join(us_counties, county_data, by=c("subregion"="County"))


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
      selectInput(inputId = "y", 
                  label = "Pollutant AQI:",
                  choices = c('NO2.AQI_mean', 'O3.AQI_mean', 'SO2.AQI_mean', 'CO.AQI_mean'), 
                  selected = "NO2.AQI"),
                  
                  
                  # # Set alpha level ---------------------------------------------
                  # sliderInput(inputId = "alpha", 
                  #             label = "Alpha:", 
                  #             min = 0, max = 1, 
                  #             value = 0.5),
                  # 
                  # # Set point size ----------------------------------------------
                  # sliderInput(inputId = "size", 
                  #             label = "Size:", 
                  #             min = 0, max = 5, 
                  #             value = 2),
                  
      
      
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
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Download Button
      downloadLink('downloadData', 'Download Data')


  ),
      
      
  # Output: -------------------------------------------------------
  mainPanel(
    
    # Show plot1 --------------------------------------------
    plotOutput(outputId = "plot1"),
    br(),        # a little bit of visual separation
    

    # Show plot2 --------------------------------------------
    # add year input (dropdown), 
    plotOutput(outputId = "plot2"),
    br(),        # a little bit of visual separation

    # Show plot3 --------------------------------------------
    plotOutput(outputId = "plot3"),
    br(),        # a little bit of visual separation

    
    # Show data table ---------------------------------------------
    DT::dataTableOutput(outputId = "datatable")
    )
  )
)
  
# Define server function required to create the scatterplot ---------
server <- function(input, output, session) {
  
  
  # Create a subset of data filtering for selected title types ------
  data_subset <- reactive({
    # return(subset(data_subset, (State == input$InputId1 & County == input$InputId2)))
    # req(input$selected_type) # ensure availablity of value before proceeding
    filter(pollution_data, State %in% input$selected_type1 & County %in% input$selected_type2)
  })
  

  
  # # Create a subset of data filtering for selected title types ------
  # data <- reactive({
  #   pollution_data
  # })
  
  # Create scatterplot object the plotOutput function is expecting --
  
  # Question: how to filter data by the state and county selection in the sidebar panel
  # Question: how to add title to the plot depending on what state and county is selected
  # https://stackoverflow.com/questions/61800901/r-shiny-subset-a-dataframe-based-on-condition-with-reactive-column-name
  

  # filter the data based on state and county selection
  # plot_data <- reactive({
  #   data_subset %>%
  #     filter(State %in% input$selected_type1) %>%
  #     filter(County %in% input$selected_type2)
    
    # data_subset %>% filter(State == input$selected_type1 & County == input$selected_type2)
    
    # dplyr::filter(data_subset, State == input$selected_type1 & County == input$selected_type2)
  # })
  
  plot_data <- reactive({
      a <- subset(data_subset(), County == input$selected_type2, 
                  select=c(State:CO.AQI_mean))
      return(a)
      
    })
  
  map_data <- reactive({
    county_data_map <- left_join(us_counties, county_data, by=c("subregion"="County")) #add a filter here
    return(county_data_map)
  })
  
  barchart_data <- reactive({
    barchart_data <- state_data
    return(barchart_data)
  })
  
  #line chart
  output$plot1 <- renderPlot({
    ggplot(plot_data(), aes_string(x = plot_data()$Year, y = input$y)) + 
      geom_line(color = "deepskyblue3", size = 2) +
      labs(x = "Year",
           y = input$y) #gsub function, tools library(to title case)
    })
  
  #county map (how to flip color gradient? how to change y-axis label depending on selection?)
  output$plot2 <- renderPlot({
    map_data() %>%
      ggplot(aes_string(x=map_data()$long,y=map_data()$lat,group=map_data()$group, fill = input$y)) +
      geom_polygon(color = "gray90", size = 0.1) +
      coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
      #scale_fill_continuous()+
      #scale_fill_manual(values = rev(brewer.pal(3, "BuPu"))) +
      #scale_fill_brewer(palette="BuPu", direction=-1) +
      theme(legend.position="bottom",
            axis.line=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid=element_blank())
    
    
  })
  
  #bar chart
  output$plot3 <- renderPlot({
    ggplot(barchart_data(), aes_string(x=barchart_data()$Year, y=input$y)) + 
      geom_bar(stat = "identity", fill = "deepskyblue3") + coord_flip() + facet_wrap(~barchart_data()$State, ncol = 6)
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
  