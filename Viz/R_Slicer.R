library(readr)
library(tidyverse)
library(dplyr)
library(shiny)

# Load your data
data <- read.csv("C:/Users/GINE/OneDrive - Loyalist College/Desktop/AI_DataScience/term2/steps 2 - weekly check in/IDA.csv")

ui <- fluidPage(
  titlePanel("Illinois District"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Types", "Select Type", choices = unique(data$Types)),
      uiOutput("Categories"),
      uiOutput("Sub_Category")
    ),
    mainPanel(
      textOutput("selected_value"),
      dataTableOutput("data_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  output$Categories <- renderUI({
    req(input$Types)
    filtered_categories <- data$Categories[data$Types == input$Types]
    selectInput("Categories", "Select Categories", choices = unique(filtered_categories))
  })
  
  output$Sub_Category <- renderUI({
    req(input$Categories)
    print(input$Categories)  # Debug: Check the selected category
    filtered_sub_categories <- unique(data$Sub_Category[data$Categories == input$Categories])
    print(filtered_sub_categories)  # Debug: Check the filtered sub-categories
    selectInput("Sub_Category", "Select Sub-Categories", choices = filtered_sub_categories)
  })
  
  output$selected_value <- renderText({
    selected_type <- ifelse(is.null(input$Types), "None", input$Types)
    selected_category <- ifelse(is.null(input$Categories), "None", input$Categories)
    selected_sub_category <- ifelse(is.null(input$Sub_Category), "None", input$Sub_Category)
    
    paste("Selected Type:", input$Types, 
          "Categories:", input$Categories, 
          "Sub_Category:", input$Sub_Category)
  })
  output$data_table <- renderDataTable({
    req(input$Types, input$Categories, input$Sub_Category)  # Ensure all inputs are selected
    
    # Filter the data based on selections
    filtered_data <- data %>%
      filter(Types == input$Types,
             Categories == input$Categories,
             Sub_Category == input$Sub_Category)
    
    # Return the filtered data for the table
    datatable(filtered_data)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
