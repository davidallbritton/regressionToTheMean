library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

options(shiny.reactlog = TRUE)


# Define UI
ui <- fluidPage(
  titlePanel("True Scores and Test Scores"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("signal_sd", "Signal SD", min = 0, max = 30, value = 15),  # New Signal SD slider
      sliderInput("noise_sd", "Noise SD", min = 0, max = 30, value = 15),    # Updated default value to 15
      actionButton("regenerate_test1", "Regenerate All"),
      actionButton("regenerate_test2", "Regenerate Test2"),
      tableOutput("scoresTable")
    ),
    mainPanel(
      plotOutput("boxPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store the scores
  scores <- reactiveValues(TrueScores = NULL, Test1 = NULL, Test2 = NULL, Subject = NULL, correlation = NULL)
  
  # Function to generate and sort scores
  generate_scores <- function() {
    TrueScores <- rnorm(26, mean = 100, sd = input$signal_sd)  # Use Signal SD slider value here
    Test1 <- TrueScores + rnorm(26, mean = 0, sd = input$noise_sd)
    Test2 <- TrueScores + rnorm(26, mean = 0, sd = input$noise_sd)
    
    # Sort by Test1 and assign letters based on sorted order
    df <- data.frame(TrueScores = TrueScores, Test1 = Test1, Test2 = Test2) %>%
      arrange(desc(Test1)) %>%
      mutate(Subject = LETTERS[1:26]) %>%  # Assign letters in sorted order
      arrange(Subject)  # Reorder by Subject for display consistency
    
    df
  }
  
  # # Initial score generation
  # observe({
  #   scores$data <- generate_scores()
  #   scores$correlation <- cor(scores$data$Test1, scores$data$Test2)
  # })
  
  # Define a reactive value to act as a flag for initial load
  initial_load <- reactiveVal(TRUE)
  
  # Initial score generation that only runs once
  observe({
    if (initial_load()) {
      scores$data <- generate_scores()
      scores$correlation <- cor(scores$data$Test1, scores$data$Test2)
      initial_load(FALSE)  # Set the flag to FALSE after the first run
    }
  })
  
  
  # Regenerate Test1 and update data when the button is pressed
  observeEvent(input$regenerate_test1, {
    scores$data <- generate_scores()
    scores$correlation <- cor(scores$data$Test1, scores$data$Test2)
  })
  
  # Regenerate Test2 while keeping sorted labels based on initial Test1 sorting
  observeEvent(input$regenerate_test2, {
    scores$data <- scores$data %>%
      mutate(Test2 = scores$data$TrueScores + rnorm(26, mean = 0, sd = input$noise_sd))
    scores$correlation <- cor(scores$data$Test1, scores$data$Test2)
  })
  
  # Display scores in a table with coloblue letters for highest and lowest scores
  output$scoresTable <- renderTable({
    df <- scores$data
    
    # Identify the highest and lowest scores in Test1
    df <- df %>%
      mutate(Color = case_when(
        Test1 %in% tail(sort(Test1), 5) ~ "blue",
        Test1 %in% head(sort(Test1), 5) ~ "#FF00FF",
        TRUE ~ "black"
      ))
    
    # Apply HTML styling for colors in the table
    df %>% 
      mutate(Subject = sprintf("<span style='color:%s;'>%s</span>", Color, Subject)) %>%
      select(Subject, TrueScores, Test1, Test2)  # Drop Color column for display
  }, sanitize.text.function = identity)  # Allow HTML styling in the table
  
  
  # Plot only the data points without the box plot
  output$boxPlot <- renderPlot({
    # Prepare data with colors
    df <- scores$data %>%
      mutate(Color = case_when(
        Test1 %in% tail(sort(Test1), 5) ~ "blue",
        Test1 %in% head(sort(Test1), 5) ~ "#FF00FF",
        TRUE ~ "gray"
      ))
    
    # Calculate means for groups A-E and V-Z
    mean_A_E_test1 <- mean(df %>% filter(Subject %in% LETTERS[1:5]) %>% pull(Test1))
    mean_A_E_test2 <- mean(df %>% filter(Subject %in% LETTERS[1:5]) %>% pull(Test2))
    mean_V_Z_test1 <- mean(df %>% filter(Subject %in% LETTERS[22:26]) %>% pull(Test1))
    mean_V_Z_test2 <- mean(df %>% filter(Subject %in% LETTERS[22:26]) %>% pull(Test2))
    
    # Reshape data for plotting
    df_long <- df %>%
      pivot_longer(cols = c("Test1", "Test2"), names_to = "Test", values_to = "Score")
    
    # Create the plot with only data points and labels
    ggplot(df_long, aes(x = Test, y = Score)) +
      geom_text(aes(label = Subject, color = Color), position = position_jitter(width = 0.15, height = 0)) +
      geom_hline(yintercept = 100, color = "yellow", linetype = "solid") +  # Add solid yellow line at 100
      # Add big blue dot for the mean of A-E and matching color for V-Z
      geom_point(aes(x = "Test1", y = mean_A_E_test1), color = "blue", size = 5) +
      geom_point(aes(x = "Test2", y = mean_A_E_test2), color = "blue", size = 5) +
      geom_point(aes(x = "Test1", y = mean_V_Z_test1), color = "#FF00FF", size = 5) +
      geom_point(aes(x = "Test2", y = mean_V_Z_test2), color = "#FF00FF", size = 5) +
      scale_color_identity() +
      labs(
        title = paste("Correlation of Test1 and Test2, r =", round(scores$correlation, 2)),
        x = "Test", y = "Score"
      ) +
      theme_minimal()
  })
  

  
  # # Plot only the data points without the box plot
  # output$boxPlot <- renderPlot({
  #   # Prepare data with colors
  #   df <- scores$data %>%
  #     mutate(Color = case_when(
  #       Test1 %in% tail(sort(Test1), 5) ~ "blue",
  #       Test1 %in% head(sort(Test1), 5) ~ "#FF00FF",
  #       TRUE ~ "gray"
  #     ))
  #   
  #   # Reshape data for plotting
  #   df_long <- df %>%
  #     pivot_longer(cols = c("Test1", "Test2"), names_to = "Test", values_to = "Score")
  #   
  #   # Create the plot with only data points and labels
  #   ggplot(df_long, aes(x = Test, y = Score)) +
  #     geom_text(aes(label = Subject, color = Color), position = position_jitter(width = 0.15, height = 0)) +
  #     geom_hline(yintercept = 100, color = "yellow", linetype = "solid") +  # Add solid blue line at 100
  #     scale_color_identity() +
  #     labs(
  #       title = paste("Correlation of Test1 and Test2, r =", round(scores$correlation, 2)),
  #       x = "Test", y = "Score"
  #     ) +
  #     theme_minimal()
  # })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
