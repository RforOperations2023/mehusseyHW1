library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("Environmental Impact of Food Production"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("column", "Select a measure to display:", 
                  choices = c("Total Emissions" = "total_emissions", 
                              "Greenhouse Gas Emissions" = "greenhouse_gas", 
                              "Land Use Emissions" = "land_use"),
                  selected = "Total Emissions")),
    mainPanel(
      plotOutput("bar_chart"),
      DT::dataTableOutput("data_table")
    )
  )
)
server <- function(input, output) {
  
  food_filtered <- reactive({
    req(input$column)
    filtered_data <- food %>%
      select(product, input$column) 
  })
  
# Render the bar chart
output$bar_chart <- renderPlot({
  ggplot(food, aes_string(x = "product", y = input$column, color = "purple")) +
    geom_bar(stat = "identity") +
    xlab("Product") +
    ylab("Total") +
    ggtitle("Product Totals")
})
  
  output$table <- DT::renderDataTable({
    DT::datatable(food_filtered())
  })
}
shinyApp(ui, server)