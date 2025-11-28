library(shiny)
library(ggplot2)
library(gridExtra)

# Function to simulate compound process S(t)
simulate_S_t <- function(t, lambda, mu, n_sim = 10000) {
  results <- numeric(n_sim)
  
  for(i in 1:n_sim) {
    # Number of events follows Poisson process
    n_events <- rpois(1, lambda * t)
    
    if(n_events > 0) {
      # Sum of exponential claims
      claims <- rexp(n_events, rate = mu)
      results[i] <- sum(claims)
    } else {
      results[i] <- 0
    }
  }
  
  return(results)
}

# UI definition
ui <- fluidPage(
  titlePanel("Compound Process S(t) - Histogram Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Process Parameters"),
      sliderInput("lambda", "Interarrival Rate (λ):",
                  min = 0.1, max = 5, value = 1, step = 0.1),
      p("Higher λ = more frequent arrivals", style = "color: gray; font-size: 12px;"),
      
      sliderInput("mu", "Claim Size Rate (μ):",
                  min = 0.1, max = 5, value = 1, step = 0.1),
      p("Higher μ = smaller individual claims", style = "color: gray; font-size: 12px;"),
      
      numericInput("n_sim", "Number of Simulations:",
                   value = 10000, min = 1000, max = 50000, step = 1000),
      actionButton("run", "Generate Histograms", class = "btn-primary"),
      
      hr(),
      h4("Theoretical Results:"),
      verbatimTextOutput("theory_results"),
      
      hr(),
      h4("Parameter Impact Summary:"),
      verbatimTextOutput("param_impact")
    ),
    
    mainPanel(
      h3("Histograms of S(t) at t = 10, 100, 1000, 10000"),
      plotOutput("histograms", height = "800px"),
      
      hr(),
      h3("Distribution Statistics"),
      tableOutput("stats_table")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Reactive simulation data
  sim_data <- eventReactive(input$run, {
    time_points <- c(10, 100, 1000, 10000)
    
    results <- list()
    for(t in time_points) {
      data <- simulate_S_t(t, input$lambda, input$mu, input$n_sim)
      results[[as.character(t)]] <- data.frame(
        Time = t,
        S_t = data
      )
    }
    
    return(results)
  })
  
  # Theoretical results
  output$theory_results <- renderText({
    time_points <- c(10, 100, 1000, 10000)
    
    theory_text <- ""
    for(t in time_points) {
      mean_theory <- (input$lambda * t) / input$mu
      var_theory <- (2 * input$lambda * t) / (input$mu^2)
      theory_text <- paste0(theory_text, 
                            "t = ", t, ":\n",
                            "  E[S(t)] = ", round(mean_theory, 2), "\n",
                            "  Var[S(t)] = ", round(var_theory, 2), "\n\n")
    }
    theory_text
  })
  
  # Parameter impact explanation
  output$param_impact <- renderText({
    paste("λ (arrival rate) impact:\n",
          "- Increases both mean and variance\n",
          "- More frequent claims → higher S(t)\n\n",
          "μ (claim size) impact:\n", 
          "- Higher μ = smaller claims\n",
          "- Reduces mean and variance\n",
          "- Effect on variance is squared (1/μ²)")
  })
  
  # Histogram plot
  output$histograms <- renderPlot({
    data_list <- sim_data()
    time_points <- c(10, 100, 1000, 10000)
    
    plots <- list()
    
    for(i in 1:length(time_points)) {
      t <- time_points[i]
      df <- data_list[[as.character(t)]]
      
      # Calculate theoretical mean
      mean_theory <- (input$lambda * t) / input$mu
      
      # Remove extreme outliers for better visualization (keep 99th percentile)
      q99 <- quantile(df$S_t, 0.99)
      df_filtered <- df[df$S_t <= q99, ]
      
      p <- ggplot(df_filtered, aes(x = S_t)) +
        geom_histogram(aes(y = ..density..), bins = 50, 
                       fill = "lightblue", color = "black", alpha = 0.7) +
        geom_vline(xintercept = mean_theory, color = "red", 
                   linetype = "dashed", size = 1) +
        labs(title = paste("t =", t),
             subtitle = paste("Theoretical mean:", round(mean_theory, 2)),
             x = "S(t)",
             y = "Density") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12))
      
      plots[[i]] <- p
    }
    
    # Arrange in 2x2 grid
    grid.arrange(grobs = plots, ncol = 2, 
                 top = paste("Compound Process S(t) Distribution\n",
                             "λ =", input$lambda, ", μ =", input$mu))
  })
  
  # Statistics table
  output$stats_table <- renderTable({
    data_list <- sim_data()
    time_points <- c(10, 100, 1000, 10000)
    
    stats_df <- data.frame()
    
    for(t in time_points) {
      data <- data_list[[as.character(t)]]
      mean_theory <- (input$lambda * t) / input$mu
      var_theory <- (2 * input$lambda * t) / (input$mu^2)
      
      stats_row <- data.frame(
        Time = t,
        Theoretical_Mean = round(mean_theory, 2),
        Simulated_Mean = round(mean(data$S_t), 2),
        Theoretical_Variance = round(var_theory, 2),
        Simulated_Variance = round(var(data$S_t), 2),
        P_S_t_0 = round(mean(data$S_t == 0), 4)
      )
      
      stats_df <- rbind(stats_df, stats_row)
    }
    
    stats_df
  }, digits = 4)
}

# Run the application
shinyApp(ui = ui, server = server)
