library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Define UI
ui <- fluidPage(
  titlePanel("Claims Data and Input Parameters"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("loss_year", "Loss Year:", value = NULL),
      numericInput("dev_year", "Development Year:", value = NULL),
      numericInput("claims_paid", "Amount of Claims Paid:", value = NULL),
      actionButton("add_button", "Add Entry"),
      hr(),
      numericInput("tail_factor", "Tail Factor:", value = NULL),
      actionButton("add_tail_factor_button", "Add Tail Factor"),
      actionButton("clear_button", "Clear Inputs")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Claims Data", tableOutput("claims_table")),
        tabPanel("Input Parameters", tableOutput("parameters_table")),
        tabPanel("Cumulative Paid Claims", tableOutput("cumulative_table")),
        tabPanel("Cumulative Paid Claims Chart", plotOutput("cumulative_plot"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  
  
  claims_data <- reactiveVal(data.frame(LossYear = numeric(0), DevYear = numeric(0), ClaimsPaid = numeric(0)))
  params_data <- reactiveVal(data.frame(TailFactor = numeric(0)))
  
  
  observeEvent(input$add_button, {
    new_entry <- data.frame(
      LossYear = input$loss_year,
      DevYear = input$dev_year,
      ClaimsPaid = input$claims_paid
    )
    
    if (any(claims_data()$LossYear == new_entry$LossYear & claims_data()$DevYear == new_entry$DevYear)) {
      claims_data_df <- claims_data()
      claims_data_df[claims_data_df$LossYear == new_entry$LossYear & claims_data_df$DevYear == new_entry$DevYear, "ClaimsPaid"] <-
        claims_data_df[claims_data_df$LossYear == new_entry$LossYear & claims_data_df$DevYear == new_entry$DevYear, "ClaimsPaid"] +
        new_entry$ClaimsPaid
      claims_data(claims_data_df)
    } else {
      claims_data(rbind(claims_data(), new_entry))
    }
  })
  
  observeEvent(input$add_tail_factor_button, {
    params_data(data.frame(TailFactor = input$tail_factor))
  })
  
  observeEvent(input$clear_button, {
    claims_data(data.frame(LossYear = numeric(0), DevYear = numeric(0), ClaimsPaid = numeric(0)))
    params_data(data.frame(TailFactor = numeric(0)))
  })
  
  
  cumulative_data <- reactive({
    
    claims <- claims_data()
    tail_factor <- params_data()$TailFactor
    
    if (!is.null(claims) && !is.null(tail_factor)) {
      unique_loss_years <- unique(claims$LossYear)
      unique_dev_years <- unique(claims$DevYear)
      
      # Calculate the largest development year
      largest_dev_year <- max(unique_dev_years)
      
      # Create cumulative_matrix with an additional column
      cumulative_matrix <- matrix(0, nrow = length(unique_loss_years), ncol = length(unique_dev_years) + 2)  # +2 for the additional column
      colnames(cumulative_matrix) <- c("Loss Year", paste("Development Year", unique_dev_years), paste("Development Year", largest_dev_year+1))
      
      for (i in 1:length(unique_loss_years)) {
        loss_year <- unique_loss_years[i]
        claims_subset <- claims[claims$LossYear == loss_year, ]
        dev_years <- unique(claims_subset$DevYear)
        
        for (j in 1:length(dev_years)) {
          dev_year <- dev_years[j]
          subset <- claims_subset[claims_subset$DevYear <= dev_year, ]
          cumulative_matrix[i, j + 1] <- sum(subset$ClaimsPaid)
        }
        
        cumulative_matrix[i, 1] <- round(loss_year)
        
        cumulative_matrix[3, 3] <- round(((cumulative_matrix[1, 3] + cumulative_matrix[2, 3]) / (cumulative_matrix[1, 2] + cumulative_matrix[2, 2])) * cumulative_matrix[3, 2])
        cumulative_matrix[2, 4] <- round((cumulative_matrix[1, 4] / cumulative_matrix[1, 3]) * cumulative_matrix[2, 3])
        cumulative_matrix[3, 4] <- round((cumulative_matrix[1, 4] / cumulative_matrix[1, 3]) * cumulative_matrix[3, 3])
        
        cumulative_matrix[1, 5] <- round(cumulative_matrix[1, 4] * tail_factor)
        cumulative_matrix[2, 5] <- round(cumulative_matrix[2, 4] * tail_factor)
        cumulative_matrix[3, 5] <- round(cumulative_matrix[3, 4] * tail_factor)
      }
      
      data.frame(cumulative_matrix)
    } else {
      NULL
    }
  })
  
  output$claims_table <- renderTable({
    claims_data()
  })
  
  output$params_table <- renderTable({
    params_data()
  })
  
  output$cumulative_table <- renderTable({
    cumulative_data()
  })
  
  output$cumulative_plot <- renderPlot({
    
    cumulative_matrix_data <- cumulative_data()  # Get the cumulative matrix data
    
    #convert matrix to data frame
    df <- as.data.frame(cumulative_matrix_data)
    
    #view structure of data frame
    str(df)
    
    # Convert dataframe to long format for plotting
    data_long <- df %>%
      pivot_longer(cols = starts_with("Development.Year"),
                   names_to = "Development_Year",
                   values_to = "Cumulative_Paid_Claims")
    
    # Convert "Development Year" to numeric
    data_long$Development_Year <- as.numeric(gsub("Development.Year.", "", data_long$Development_Year))
    
    # Plot the data using ggplot
    ggplot(data = data_long, aes(x = Development_Year, y = Cumulative_Paid_Claims, color = factor(Loss.Year))) +
      geom_line() +
      geom_point() +
      labs(x = "Development Year", y = "Cumulative Paid Claims",
           title = "Cumulative Paid Claims by Development Year",
           color = "Loss Year") +
      scale_color_discrete(name = "Loss Year")
  })
  
}

shinyApp(ui, server)