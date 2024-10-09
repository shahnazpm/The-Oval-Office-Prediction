# Import Libraries
install.packages("ggplot2")
install.packages("tidyverse")  # This includes dplyr, tidyr, etc.
install.packages("shiny")
install.packages("readxl")  # Only run this line if you haven't installed the package

library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(readxl)

# Load the new dataset
Illinois <- read.csv("C:/Users/GINE/OneDrive - Loyalist College/Desktop/AI_DataScience/term2/R/Illinois_Hawaii/Illinois.csv")
View(Illinois)

# Check the structure and column names
print(str(Illinois))  # View the structure of the dataset
print(colnames(Illinois))  # View column names

# Define server logic required to output a boxplot
server <- function(input, output, session) {
  # Use the Illinois dataset
  nc1 <- Illinois  # Replace the dataset with Illinois
  
  # Exclude MOE columns (if applicable, adjust the column names based on your dataset)
  nc1 <- nc1[, !grepl("MOE$", names(nc1))]
  
  # Remove rows with totals (adjust according to your dataset's structure)
  nc1 <- nc1[!grepl("Total population|All people", nc1$LeadingParty), ]
  
  # Convert relevant columns to numeric if necessary (adjust based on your dataset)
  nc1$Count <- as.numeric(as.character(nc1$Count))
  
  # Create a reactive data frame based on user selection
  data <- reactive({
    req(input$sel_Variable)  # Ensure a variable is selected
    df <- nc1 %>%
      group_by(across(all_of(input$sel_Variable))) %>%  # Group by the selected variable
      summarise(Population = sum(Count, na.rm = TRUE), .groups = 'drop')  # Sum population (Count)
    df  # Return df at the end of the reactive block
  })
  
  # Update the select input choices based on column names
  observe({
    updateSelectInput(session, "sel_Variable", choices = colnames(nc1)[-which(names(nc1) == "Count")])   # Exclude Count from choices
  
  
  # Update choices for the second variable for scatter plot
  updateSelectInput(session, "sel_Variable2", choices = colnames(nc1)[sapply(nc1, is.numeric)])  # Only numeric columns
  })

# Create the plot
output$plot <- renderPlot({
  req(data())  # Ensure that data is available for plotting
  ggplot(data(), aes_string(x = input$sel_Variable, y = "Population")) +
    geom_col() +  # Use geom_col to create a bar plot
    labs(x = input$sel_Variable, y = "Population Count") +
    theme_minimal() +  # A clean theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve x-axis text readability
})

# Additional Plots
output$bar_plot_race <- renderPlot({
  ggplot(nc1, aes(x = Race)) +
    geom_bar(aes(y = Count), stat = "identity", fill = "skyblue") +
    labs(title = "Count of Individuals by Race", x = "Race", y = "Count") +
    theme_minimal()
})

output$pie_plot_gender <- renderPlot({
  gender_counts <- nc1 %>%
    group_by(Gender) %>%
    summarise(Count = n())
  
  ggplot(gender_counts, aes(x = "", y = Count, fill = Gender)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Distribution of Gender") +
    theme_void()
})

output$stacked_bar_health <- renderPlot({
  ggplot(nc1, aes(x = EmploymentStatus, fill = HealthCoverage)) +
    geom_bar(position = "fill") +
    labs(title = "Health Coverage by Employment Status", y = "Proportion") +
    theme_minimal()
})

# Check the summary of IncomeNumeric to understand how many NAs you have
summary(nc1$IncomeNumeric)
na_count <- sum(is.na(nc1$IncomeNumeric))
cat("Number of NA values in IncomeNumeric:", na_count, "\n")

# Identify rows with unmatched values
unmatched_values <- nc1[is.na(nc1$IncomeNumeric), "TotalHouseholdIncome"]
cat("Unmatched values in TotalHouseholdIncome:", unique(unmatched_values), "\n")

# Update recode with additional cases if necessary
nc1$IncomeNumeric <- recode(nc1$TotalHouseholdIncome,
                            "$10,000-$14,999" = 12000,
                            "$15,000-$24,999" = 19500,
                            "$25,000-$34,999" = 29500,
                            "$35,000-$49,999" = 42000,
                            "$50,000-$74,999" = 62000,
                            "$75,000-$99,999" = 87000,
                            "$100,000-$149,999" = 125000,
                            "$150,000-$199,999" = 175000,
                            "$200,000 or more" = 200000,  # Add this if you find such cases
                            .default = NA_real_)  # Replace unmatched values with NA

output$box_plot_income <- renderPlot({
  ggplot(nc1 %>% filter(!is.na(IncomeNumeric)), aes(x = Race, y = IncomeNumeric)) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = "Income Distribution by Race", y = "Income ($)", x = "Race") +
    theme_minimal()
})



output$histogram_votes <- renderPlot({
  ggplot(nc1, aes(x = TargetVotes)) +
    geom_histogram(binwidth = 1000000, fill = "lightblue", color = "black") +
    labs(title = "Distribution of Target Votes", x = "Target Votes", y = "Frequency") +
    theme_minimal()
})

# Create scatter plot output
output$scatter_plot <- renderPlot({
  req(input$sel_Variable2)  # Ensure a second variable is selected
  ggplot(nc1, aes_string(x = input$sel_Variable, y = input$sel_Variable2)) +
    geom_point(alpha = 0.6, color = "blue") +
    labs(x = input$sel_Variable, y = input$sel_Variable2, title = paste("Scatter Plot of", input$sel_Variable, "vs", input$sel_Variable2)) +
    theme_minimal()
})
}

ui <- basicPage(
  h4("Dynamic Comparison of Population by Selected Variable"),
  selectInput(inputId = "sel_Variable",
              label = "Choose Variable",
              choices = NULL,  # Choices will be populated dynamically
              multiple = FALSE  # Single selection for better clarity
  ),
  selectInput(inputId = "sel_Variable2",
              label = "Choose Second Variable for Scatter Plot",
              choices = NULL,  # Choices will be populated dynamically
              multiple = FALSE  # Single selection for clarity
  ),
  plotOutput("plot"),
  plotOutput("bar_plot_race"),       # Count of Individuals by Race
  plotOutput("pie_plot_gender"),      # Distribution of Gender
  plotOutput("stacked_bar_health"),   # Health Coverage by Employment Status
  plotOutput("box_plot_income"),      # Income Distribution by Race
  plotOutput("histogram_votes"),      # Distribution of Target Votes
  plotOutput("scatter_plot")          # Scatter plot output
)

# Run the application 
shinyApp(ui = ui, server = server)
