library(shiny)

ui <- fluidPage(
  titlePanel("Z-Test Hypothesis Test"),
  sidebarLayout(
    sidebarPanel(
      numericInput("hypothesized_value", "Hypothesized Value", value = 5),
      numericInput("significance_level", "Significance Level", value = 0.05),
      actionButton("run_test", "Run Test")
    ),
    mainPanel(
      verbatimTextOutput("results")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$run_test, {
    # Sample data
    data <- c(4.8, 5.1, 5.3, 4.9, 5.2, 5.0, 4.7, 5.3, 5.1, 5.0)
    
    # Hypothesized value
    hypothesized_value <- input$hypothesized_value
    
    # Significance level (α)
    significance_level <- input$significance_level
    
    # Calculate the sample mean and standard deviation
    sample_mean <- mean(data)
    sample_sd <- sd(data)
    
    # Calculate the standard error (assuming population standard deviation is known)
    n <- length(data)
    standard_error <- sample_sd / sqrt(n)
    
    # Calculate the z-statistic
    z_statistic <- (sample_mean - hypothesized_value) / standard_error
    
    # Determine the critical region (two-tailed test)
    critical_value <- qnorm(1 - (significance_level / 2))
    
    # Calculate the p-value (two-tailed test)
    p_value <- 2 * (1 - pnorm(abs(z_statistic)))
    
    # Make a decision based on the p-value and significance level
    if (p_value < significance_level) {
      decision <- "Reject the null hypothesis"
    } else {
      decision <- "Fail to reject the null hypothesis"
    }
    
    # Display the results
    output$results <- renderPrint({
      cat("Z-Test Results:\n")
      cat("Sample mean:", sample_mean, "\n")
      cat("Hypothesized value:", hypothesized_value, "\n")
      cat("Z-statistic:", z_statistic, "\n")
      cat("Critical value:", critical_value, "\n")
      cat("P-value:", p_value, "\n")
      cat(decision, "\n")
    })
  })
}

shinyApp(ui, server)
