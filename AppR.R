library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(shinythemes)
library(htmltools)
library(dygraphs)
library(quantmod)

# load cleaned data
load("power_plants.RData")

# color palette
fuel_pal <- colorFactor(
  palette = "Set2",
  domain = mainland_power_plants$Fuel_Category
)

ui <- navbarPage(
  title = "US Power Plants",
  theme = shinytheme("united"),
  
  tabPanel(
    "ReadMe",
    br(),
    h2("About this app"),
    p("This app explores electric power plants in the United States."),
    p("Plants are mapped by fuel type and generating capacity."),
    p("Use the second map tab to filter plants by energy source."),
    p("The Energy Stocks tab shows historical stock closing prices for selected U.S. energy companies.")
  ),
  
  tabPanel(
    "All Power Plants",
    leafletOutput("map1", height = 700)
  ),
  
  tabPanel(
    "By Energy Source",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "fuel",
          "Choose fuel type:",
          choices = levels(mainland_power_plants$Fuel_Category),
          selected = "Natural Gas"
        )
      ),
      
      mainPanel(
        leafletOutput("map2", height = 700)
      )
    )
  ),
  
  tabPanel(
    "Energy Stocks",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "stock_choice",
          label = "Choose energy companies:",
          choices = c(
            "NextEra Energy" = "NEE",
            "Duke Energy" = "DUK",
            "Southern Company" = "SO",
            "Constellation Energy" = "CEG"
          ),
          selected = c("NEE", "DUK"),
          multiple = TRUE
        ),
        
        dateRangeInput(
          inputId = "stock_dates",
          label = "Choose date range:",
          start = "2020-01-01",
          end = Sys.Date(),
          min = "2020-01-01",
          max = Sys.Date()
        )
      ),
      
      mainPanel(
        dygraphOutput("stock_graph", height = "600px")
      )
    )
  )
)

server <- function(input, output, session){
  
  output$map1 <- renderLeaflet({
    leaflet(mainland_power_plants) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        radius = ~sqrt(Total_MW)/2,
        color = ~fuel_pal(Fuel_Category),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste0(
          "<b>", Plant_Name, "</b><br>",
          "Fuel: ", Fuel_Category, "<br>",
          "Capacity: ", Total_MW, " MW<br>",
          "State: ", StateName
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = fuel_pal,
        values = ~Fuel_Category,
        title = "Fuel Type"
      )
  })
  
  filtered <- reactive({
    mainland_power_plants %>%
      filter(Fuel_Category == input$fuel)
  })
  
  output$map2 <- renderLeaflet({
    leaflet(filtered()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        radius = ~sqrt(Total_MW)/2,
        color = ~fuel_pal(Fuel_Category),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste0(
          "<b>", Plant_Name, "</b><br>",
          "Fuel: ", Fuel_Category, "<br>",
          "Capacity: ", Total_MW, " MW<br>",
          "State: ", StateName
        )
      )
  })
  
  output$stock_graph <- renderDygraph({
    req(input$stock_choice)
    
    selected_stocks <- stock_prices[, input$stock_choice]
    
    dygraph(
      selected_stocks,
      main = "Historical Closing Prices of Major U.S. Energy Companies"
    ) %>%
      dyOptions(
        drawPoints = TRUE,
        pointSize = 2,
        strokeWidth = 2
      ) %>%
      dyLegend(show = "always") %>%
      dyRangeSelector(dateWindow = input$stock_dates)
  })
}

shinyApp(ui, server)

## Published app: