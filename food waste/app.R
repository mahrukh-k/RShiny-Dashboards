library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(directlabels)
library(geomtextpath)
library(ggrepel)
library(treemap)
library(treemapify)
library(ggplotify)
library(formattable)


# Load data ----------------------------------------------
data <- read_csv('ReFED_US_Food_Surplus.csv')


# Dashboard header ----------------------------------------------
header <- dashboardHeader(title = "Dashboard")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    
    # Input: Select Sector of food supply chain ----------------------------------------------
    checkboxGroupInput("sectorinput",
                "Food Supply Chain Sector:",
                choices = sort(unique(data$sector)),
                selected = c('Farm','Foodservice','Manufacturing','Residential','Retail')),
    
    
    # Input: Select Year ----------------------------------------------
    sliderInput("yearinput",
                "Year:",
                min = min(data$year, na.rm = T),
                max = max(data$year, na.rm = T),
                value = c(min(data$year, na.rm = T), max(data$year, na.rm = T)),
                step = 1),
    
    # Input: Select food type
    selectInput("foodtypeinput",
                "Food Type:",
                choices = sort(unique(data$food_type)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Dry Goods", "Frozen", "Produce", "Prepared Foods", "Breads & Bakery", 
                             "Ready-to-drink Beverages", "Dairy & Eggs", "Fresh Meat & Seafood"))
    

  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            
            #Value box presenting total food wasted in tonnes
            valueBoxOutput("box1"),
            
            #Value box presenting total food wasted in terms of USD
            valueBoxOutput("box3")
          ),
          
          fluidRow(
            
            #Info box presenting total number of meals wasted
            infoBoxOutput("box2"),
            
            #Info box presenting total amount of food incinerated
            infoBoxOutput("box4")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plots",
                   width = 12,
                   tabPanel("Plot 1", plotlyOutput("plot1")),
                   tabPanel("Plot 2", plotlyOutput("plot2")),
                   tabPanel("Plot 3", plotlyOutput("plot3")))   
                           
            
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            
            #add source of data above the data table
            box(title = "Data (Source: ReFED Food Waste Monitor)", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  
  #reactive data for data table according to user inputs
  datainput <- reactive({
    data_app <- data %>%
      filter(year >= input$yearinput[1] & year <= input$yearinput[2]) %>%
      filter(food_type %in% input$foodtypeinput) %>%
      filter(sector %in% input$sectorinput)

    # Return dataframe ----------------------------------------------
    return(data_app)
  })
  
  
  # Plot-1 -----------------------------
  
  #create a subset of the data that averages total food wasted by sector and year
  output$plot1 <- renderPlotly({
    dp1 <- datainput() %>%
      group_by(sector, year) %>%
      summarise_at(vars(tons_waste), list(tons_waste_mean = mean))
    
    
    #use the above data subset and further filter it according to user inputs
    dp1 <- dp1 %>%
      filter(year >= input$yearinput[1] & year <= input$yearinput[2]) %>%
      filter(sector %in% input$sectorinput)
    
    #plot
    plot1 <- ggplot(data = dp1, aes(x = as.factor(year), y = tons_waste_mean, fill = sector,
                           text = paste("Year:",year,"<br>Sector:",sector,"<br>Waste:",tons_waste_mean))) +
      geom_bar(stat = 'identity') + theme_classic() + labs(x = "Year",
                                                           y = "Food Wastage by Sector.",
                                                           title = "Food Waste (Tons)")
    
    plot1 <- plot1 %>% 
      ggplotly(tooltip="text")

  })
  
  
  # Plot-2 -----------------------------------
  
  #create a subset of the data that averages total food wasted by sector and year
  output$plot2 <- renderPlotly({
    dp2 <- datainput() %>%
      group_by(food_type, year) %>%
      summarise_at(vars(tons_waste), list(food_type_mean = mean))
    
    #use the above data subset and further filter it according to user inputs
    dp2 <- dp2 %>%
      filter(year >= input$yearinput[1] & year <= input$yearinput[2]) %>%
      filter(food_type %in% input$foodtypeinput)

    #plot
    plot_ly(data = dp2,x = ~food_type, y = ~food_type_mean, type = 'bar',
            hovertemplate = ~paste("<b>Year:</b>", year, "<br><b>Food Waste<b>", comma(food_type_mean,digits=0), "kilotonnes")) %>% 
      layout(title = "Food Waste by Product Type",
             xaxis = list(title = "", tickangle = -90),
             yaxis = list(title = ""))
  })
  
  # Plot-3 -----------------------------------
  
  #create a subset of the data that averages total food wasted by sector and year
  output$plot3 <- renderPlotly({
    dp3 <- datainput() %>%
      group_by(sector, year) %>%
      summarise_at(vars(tons_surplus), list(tons_surplus_mean = mean))

    #use the above data subset and further filter it according to user inputs
    dp3 <- dp3 %>%
      filter(year == input$yearinput)

    #plot
    plot_ly(dp3, labels = ~sector, values = ~tons_surplus_mean) %>% add_pie(hole = 0.6) %>%
      layout(title = 'Surplus Food Produced by Sector')

  })
  

  # Data table ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(datainput(), select = c(year, sector, sub_sector, food_type, tons_waste)) %>%
      filter(year >= input$yearinput[1] & year <= input$yearinput[2]) %>%
      filter(food_type %in% input$foodtypeinput) %>%
      filter(sector == input$sectorinput)
  })
  

  # Value box: Tons of Food Wasted -----------------------------------------------
  output$box1 <- renderValueBox({
    valueboxdata <- datainput() %>%
      filter(year >= input$yearinput[1] & year <= input$yearinput[2])
    
    valueboxnum <- round(mean(valueboxdata$tons_waste, na.rm = T))
    
    valueBox(subtitle = "Tons of Food Wasted", value = comma(valueboxnum, digits = 0), icon = icon("drumstick-bite"), color = "blue")
    
  })
  
  
  # Info box: Meals Wasted -----------------------------------------------
  output$box2 <- renderInfoBox({
    infoboxdata <- datainput() %>%
      filter(year >= input$yearinput[1] & year <= input$yearinput[2])
    
    infoboxnum <- round(mean(infoboxdata$meals_wasted, na.rm = T))
    
    infoBox("", subtitle = "Meals Wasted", value = comma(infoboxnum, digits = 0), icon = icon("utensils"), color = "purple")
    
  })
  
  # Value box: Surplus in Dollars -----------------------------------------------
  output$box3 <- renderValueBox({
    valueboxdata <- datainput() %>%
      filter(year >= input$yearinput[1] & year <= input$yearinput[2])
    
    valueboxnum2 <- round(mean(valueboxdata$us_dollars_surplus, na.rm = T))
    
    valueBox(subtitle = "Surplus Food in U.S. Dollars", value = currency(valueboxnum2,  "USD", sep = " "), icon = icon("dollar-sign"), color = "blue")
    
  })
  
  
  # Info box: Tons of Meals Wasted -----------------------------------------------
  output$box4 <- renderInfoBox({
    infoboxdata <- datainput() %>%
      filter(year >= input$yearinput[1] & year <= input$yearinput[2])
    
    infoboxnum <- round(mean(infoboxdata$tons_incineration, na.rm = T))
    
    infoBox("", subtitle = "Tons of Food Incinerated", value = comma(infoboxnum, digits = 0), icon = icon("fire"), color = "purple")
    
  })
}

# Run the application --------------------------------------------------
shinyApp(ui = ui, server = server)