# Load the necessary libraries
library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)
library(RColorBrewer)

# Read the data
data <- read.csv("D:/Loyalist/Term2/Step2/Week6/Ohio_records.csv")

# Filter variables to include only those with more than one unique value and remove 'Count'
category_choices <- names(data)[sapply(data, function(col) length(unique(col)) > 1) & names(data) != "Count"]

# Define the UI
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .custom-title {
        font-size: 50px;  /* Change to desired size */
        color: #C1133D;   /* Optional: Change text color */
        text-align: left; /* Optional: Center the title */
      }
    .sidebar {
      background-color: #333333;
      color: white;
    }
    .help-text {
      white-space: normal; /* Allow text to wrap */
      margin-bottom: 10px; /* Space below help text */
    }
    .sidebar-panel {
      padding: 15px; /* Add padding for better spacing */
    }
  "))),
  
  theme = shinytheme("cerulean"),  # Use a Bootstrap theme
  titlePanel(title = div(class = "custom-title", "Ohio"), windowTitle = "Ohio"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",  # Add custom class for styling
      helpText(class = "help-text", "Select a category and their subcategories for visualization"),
      
      # Dropdown to select the category for subcategory comparison
      selectInput("category", "Select a Category:", 
                  choices = category_choices, 
                  selected = category_choices[1]),  # Select the first valid category
      
      # Separate Select All checkbox
      checkboxInput("selectAll", "Select All", value = TRUE),
      
      # UI output for dynamic checkboxes (based on the selected category)
      uiOutput("dynamicCheckboxes"),
      
      # Add some spacing
      tags$hr()
    ),
    
    mainPanel(
      # Plot output
      plotlyOutput("comparisonPlot", height = "600px")  # Set a specific height for the plot
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Dynamically generate checkboxes based on the selected category
  observe({
    # Get the selected category
    selected_cat <- input$category
    
    # Get unique subcategories for the selected category
    unique_subcategories <- unique(data[[selected_cat]])
    
    # Generate the checkboxes dynamically
    output$dynamicCheckboxes <- renderUI({
      checkboxGroupInput("selectedSubcategories", 
                         paste("Select Subcategories for", selected_cat), 
                         choices = unique_subcategories, 
                         selected = unique_subcategories)  # Select all subcategories by default
    })
  })
  
  # Observe the Select All checkbox and update the selected subcategories
  observeEvent(input$selectAll, {
    selected_cat <- input$category
    unique_subcategories <- unique(data[[selected_cat]])
    
    if (input$selectAll) {
      updateCheckboxGroupInput(session, "selectedSubcategories", 
                               choices = unique_subcategories, 
                               selected = unique_subcategories)  # Select all subcategories
    } else {
      updateCheckboxGroupInput(session, "selectedSubcategories", 
                               choices = unique_subcategories, 
                               selected = character(0))  # Deselect all subcategories
    }
  })
  
  # Render the plot based on the selected category and population of the selected subcategories
  output$comparisonPlot <- renderPlotly({
    # Get the selected category
    selected_cat <- input$category
    
    # Get the selected subcategories from the checkboxes
    selected_subcategories <- input$selectedSubcategories
    
    # Filter the data based on the selected subcategories
    if (!is.null(selected_subcategories) && length(selected_subcategories) > 0) {
      filtered_data <- data %>%
        filter(!!sym(selected_cat) %in% selected_subcategories) %>%
        group_by(!!sym(selected_cat)) %>%
        summarise(Population = n()) %>%
        ungroup()
      
      # Check the number of unique subcategories
      num_subcategories <- nrow(filtered_data)
      
      # Define variables that should use pie charts
      pie_chart_vars <- c("Gender", "MortgageStatus", "HealthCoverage")  # Specify the variables for pie charts
      
      # Check if the selected category is one of the specified pie chart variables
      if (selected_cat %in% pie_chart_vars && num_subcategories <= 2) {
        # Create a pie chart for two subcategories
        fig <- plot_ly(filtered_data, labels = ~as.factor(filtered_data[[selected_cat]]), 
                       values = ~Population, 
                       type = 'pie', 
                       textinfo = 'label+percent', 
                       insidetextorientation = 'radial', 
                       marker = list(colors = c("#B9D9EB", "#FFAA00")),  # Light yellow and orange colors
                       textfont = list(size = 14)  # Set font size for text in pie chart
        ) %>%
          layout(title = paste("Population Distribution by", selected_cat),
                 margin = list(l = 0, r = 0, t = 50, b = 0),  # Adjust margins
                 showlegend = TRUE)
      }
      else {
        # Standard bar chart or switch axes for more than 3 subcategories
        if (num_subcategories > 3) {
          fig <- plot_ly(
            filtered_data, 
            y = as.factor(filtered_data[[selected_cat]]),  # Switch x and y
            x = ~Population, 
            type = 'bar',
            text = ~paste0(format(Population / 1000, nsmall = 2, big.mark = ","), "k"),  # Format population in thousands with "k"
            textposition = 'auto', # Automatically position the text on bars
            marker = list(color = '#2a52be'),  # Set bar color
            hoverinfo = 'text',  # Show actual population value on hover
            hovertext = ~paste("Population: ", Population),  # Show the actual number on hover
            showlegend = FALSE
          ) %>%
            layout(
              title = paste("Comparison of Population by", selected_cat),
              xaxis = list(title = "Population (in thousands)"),  # Switch axis title
              yaxis = list(title = selected_cat),  # Switch axis title
              plot_bgcolor = 'rgba(240, 240, 240, 0.95)',  # Background color for the plot
              paper_bgcolor = 'rgba(255, 255, 255, 1)',  # White background for the entire plot area
              margin = list(l = 50, r = 20, t = 50, b = 50)  # Adjust margins
            )
        } else {
          # Standard bar chart for 3 or fewer subcategories
          fig <- plot_ly(
            filtered_data, 
            x = as.factor(filtered_data[[selected_cat]]), 
            y = ~Population, 
            type = 'bar',
            text = ~paste0(format(Population / 1000, nsmall = 2, big.mark = ","), "k"),  # Format population in thousands with "k"
            textposition = 'auto', # Automatically position the text on bars
            marker = list(color = '#0073B2'),  # Set bar color
            hoverinfo = 'text',  # Show actual population value on hover
            hovertext = ~paste("Population: ", Population),  # Show the actual number on hover
            showlegend = FALSE
          ) %>%
            layout(
              title = paste("Comparison of Population by", selected_cat),
              xaxis = list(title = selected_cat, tickangle = 45),  # Rotate x-axis labels if needed
              yaxis = list(title = "Population (in thousands)", range = c(0, max(filtered_data$Population) * 1.2)), # Scale y-axis
              plot_bgcolor = 'rgba(240, 240, 240, 0.95)',  # Background color for the plot
              paper_bgcolor = 'rgba(255, 255, 255, 1)',  # White background for the entire plot area
              margin = list(l = 50, r = 20, t = 50, b = 50)  # Adjust margins
            )
        }
      }
      
    } else {
      # If no subcategories are selected, show an empty plot
      fig <- plot_ly() %>%
        layout(
          title = "No subcategories selected",
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    }
    
    fig
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
