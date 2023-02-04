library(readr)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)
foodtop10 <- read_csv("foodtop10.csv", show_col_types = FALSE)
colnames(foodtop10)[colnames(foodtop10) == "Packging"] <- "Packaging" #Changing a misspelling
foodprod_sum <- aggregate(foodtop10$Total_emissions, by = list(foodtop10$product), sum) #for pie chart
names(foodprod_sum) <- c("product", "Total_emissions")

#USER INTERFACE SIDE
ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("Environmental Impact of Food Production"),
  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput("y", "Select a variable for the y-axis:", 
                              c("Land Use (Kg CO2)" = "Land_use",
                               "Animal Feed (Kg CO2)" = "Animal_feed",
                               "Farm (Kg CO2)" = "Farm",
                               "Processing (Kg CO2)" = "Processing",
                               "Transport (Kg CO2)" = "Transport",
                               "Packaging (Kg CO2)" = "Packaging",
                               "Retail (Kg CO2)" = "Retail",
                               "Total Emissions (Kg CO2)" = "Total_emissions",
                               "Eutrophying (per 100 kcal)" = "Eutrophying_emissions_kcal",
                               "Eutrophying (per kilogram)" = "Eutrophying_emissions_kilogram",
                               "Eutrophying (per 100g protein)" = "Eutrophying_emissions_protein",
                               "Freshwater Withdrawals (per 100 kcal)" = "Freshwater_withdrawals_kcal",
                               "Freshwater Withdrawals (per 100g protein)" = "Freshwater_withdrawals_protein",
                               "Freshwater Withdrawals (per kilogram)" = "Freshwater_withdrawals_kilogram",
                               "Greenhouse Gas (per 100 kcal)" = "Greenhouse_gas_kcal",
                               "Greenhouse Gas (per 100g protein)" = "Greenhouse_gas_protein",
                               "Land Use (per 100 kcal)" = "Land_use_kcal",
                               "Land Use (per kilogram)" = "Land_use_kilogram",
                               "Land Use (per 100g protein)" = "Land_use_protein",
                               "Scarcity Weighted Water Use (per kilogram)" = "Scarcity_water_kilogram",
                               "Scarcity Weighted Water Use (per 100g protein)" = "Scarcity_water_protein",
                               "Scarcity Weighted Water Use (per 100 kcal)" = "Scarcity_water_kcal"),
                               selected = "Land Use (Kg CO2)"),
      selectInput("x", "Select a variable for the x-axis:", 
                  c("Land Use (Kg CO2)" = "Land_use",
                               "Animal Feed (Kg CO2)" = "Animal_feed",
                               "Farm (Kg CO2)" = "Farm",
                               "Processing (Kg CO2)" = "Processing",
                               "Transport (Kg CO2)" = "Transport",
                               "Packaging (Kg CO2)" = "Packaging",
                               "Retail (Kg CO2)" = "Retail",
                               "Total Emissions (Kg CO2)" = "Total_emissions",
                               "Eutrophying (per 100 kcal)" = "Eutrophying_emissions_kcal",
                               "Eutrophying (per kilogram)" = "Eutrophying_emissions_kilogram",
                               "Eutrophying (per 100g protein)" = "Eutrophying_emissions_protein",
                               "Freshwater Withdrawals (per 100 kcal)" = "Freshwater_withdrawals_kcal",
                               "Freshwater Withdrawals (per 100g protein)" = "Freshwater_withdrawals_protein",
                               "Freshwater Withdrawals (per kilogram)" = "Freshwater_withdrawals_kilogram",
                               "Greenhouse Gas (per 100 kcal)" = "Greenhouse_gas_kcal",
                               "Greenhouse Gas (per 100g protein)" = "Greenhouse_gas_protein",
                               "Land Use (per 100 kcal)" = "Land_use_kcal",
                               "Land Use (per kilogram)" = "Land_use_kilogram",
                               "Land Use (per 100g protein)" = "Land_use_protein",
                               "Scarcity Weighted Water Use (per kilogram)" = "Scarcity_water_kilogram",
                               "Scarcity Weighted Water Use (per 100g protein)" = "Scarcity_water_protein",
                               "Scarcity Weighted Water Use (per 100 kcal)" = "Scarcity_water_kcal"),
                               selected = "Greenhouse Gas Emissions per 100 kcal"),
      #show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      #add a download button
      downloadButton("downloadData", "Download data")),
      
  #output
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Interactive Charts", 
                         fluidRow(
                  column(6, plotOutput(outputId = "scatterplot")),
                  column(6, plotOutput(outputId = "barchart"))),
                  fluidRow(
                    DT::dataTableOutput(outputId = "datatable")
                  )),
                tabPanel("Pie Chart of Total Emissions", 
                         plotOutput(outputId = "piechart")))
    )
  )
)

#SERVER side
server <- function(input, output) {
  food_filtered <- reactive({
    req(input$y, input$x)
    foodtop10 %>% 
      arrange(desc(!!sym(input$y))) #arrange by selected y (also what they will see on bar chart)
    })
  
  #Render the scatter plot 
  output$scatterplot <- renderPlot({
    ggplot(data = foodtop10, aes_string(x = input$x, y = input$y, color = "product")) +
      geom_point(size = 3) +
      labs(x = tools::toTitleCase(str_replace_all(input$x, "\\.", " ")),
           y = tools::toTitleCase(str_replace_all(input$y, "\\.", " "))
           ) +
      theme_classic() +
      theme(legend.position = "bottom")
         })
  # Render the bar chart
  output$barchart <- renderPlot({
    ggplot(data = foodtop10, aes_string(x = "product", y = input$y, fill = "product")) +
      geom_bar(stat = "identity") +
      xlab("Food Product") +
      ylab(tools::toTitleCase(str_replace_all(input$y, "\\.", " "))) +
      ggtitle("Contribution of Food Products to Selected Emission") +
      scale_fill_brewer(palette = "Paired") +
      theme_classic() + 
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  # Render the pie chart
  output$piechart <- renderPlot({
    ggplot(data = foodprod_sum, aes(x = "", y = Total_emissions, fill = product)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Paired") +
      ggtitle("Total Emissions by Food Product") +
      xlab("") +
      ylab("Emissions") +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_legend(title = "Food Product")) + 
      theme_classic() +
      theme(axis.ticks = element_blank()) +
      theme(axis.text = element_blank())
  })
  
  output$datatable <- DT::renderDataTable(
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