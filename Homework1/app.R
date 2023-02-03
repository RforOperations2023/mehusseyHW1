library(readr)
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
food <- read_csv("food.csv")

#USER INTERFACE SIDE
ui <- fluidPage(
  titlePanel("Environmental Impact of Food Production"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "y",
                  label = "Select a measure to display:", 
                  choices = c("Total Emissions" = "total_emissions", 
                              "Greenhouse Gas Emissions" = "greenhouse_gas", 
                              "Land Use Emissions" = "land_use"),
                  selected = "Total Emissions")),
    
    #show data table
    checkboxInput(inputId = "show_data",
                  label = "Show data table",
                  value = TRUE),
    
    #add a download button
    downloadButton("downloadData", "Download data")
  ),

  #output
  mainPanel(
    plotOutput(outputId = "bar_chart"),
    plotOutput(outputId = "scatter1"),
    plotOutput(outputId = "scatter2"),
    DT::dataTableOutput(outputId = "data_table")
  )
)
#SERVER side
server <- function(input, output) {
  
  food_filtered <- reactive({
    req(input$column)
    food %>% 
      arrange(desc(!!sym(input$column))) 
  })
  
  # Render the bar chart
  output$bar_chart <- renderPlot({
    ggplot(food, aes_string(x = "product", y = input$y, color = "product")) +
      geom_bar(stat = "identity") +
      xlab("Food type") +
      ylab("Total") +
      ggtitle(input$column, "for different types of food production")
  })
  
  #Render the first scatter plot 
  output$scatter1 <- renderPlot({
    ggplot(food, aes(x = Total_emissions, y = land_use, color = product)) +
      geom_point() +
      xlab("Total Emissions") +
      ylab("Land Use Emissions (per 100kcal)") +
      ggtitle("Total Emissions vs Land Use Emissions by Food Production")
  })
  
  # Render the second scatter plot
  output$scatter2 <- renderPlot({
    ggplot(food, aes(x = Total_emissions, y = greenhouse_gas, color = product)) +
      geom_point() +
      xlab("Total Emissions") +
      ylab("Greenhouse Gas Emissions (per 100kcal)") +
      ggtitle("Total Emissions vs Greenhouse Gas Emissions by Food Production")
  })
  
  output$data_table <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = food_filtered(), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("food-data-", Sys.Date(), ".csv", sep = "")
    },content = function(file) {
      write.csv(food, file)
    }
  )
}
shinyApp(ui, server)