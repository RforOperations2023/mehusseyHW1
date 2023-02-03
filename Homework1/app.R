library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Food Product Emissions"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("product", "Select a product:", 
                   c("Beef (beef herd)", "Beef (dairy herd)", "Dark Chocolate", "Lamb & Mutton", "Cheese"), 
                   selected = "Beef (beef herd)"),
      actionButton("refresh", "Refresh")
    ),
    
    mainPanel(
      plotOutput("histogram1"),
      plotOutput("histogram2"),
      plotOutput("histogram3")
    )
  )
)

server <- function(input, output) {
  
  food_filtered <- reactive({
    food %>%
      filter(product == input$product)
  })
  
  output$histogram1 <- renderPlot({
    ggplot(food_filtered(), aes(x = Total_emissions)) +
      geom_histogram(binwidth = 1) +
      ggtitle(paste("Histogram of Total Emissions for", input$product))
  })
  
  output$histogram2 <- renderPlot({
    ggplot(food_filtered(), aes(x = Greenhouse_gas)) +
      geom_histogram(binwidth = 1) +
      ggtitle(paste("Histogram of Greenhouse Gas Emissions for", input$product))
  })
  
  output$histogram3 <- renderPlot({
    ggplot(food_filtered(), aes(x = Land_use)) +
      geom_histogram(binwidth = 1) +
      ggtitle(paste("Histogram of Land Use for", input$product))
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(food_filtered())
  })
}

shinyApp(ui, server)