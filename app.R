library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define UI
ui <- fluidPage(
  titlePanel("True Scores and Test Scores"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("noise_sd", "Noise SD", min = 0, max = 30, value = 10),
      actionButton("regenerate_test1", "Regenerate Test1"),
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
    TrueScores <- rnorm(26, mean = 100, sd = 15)
    Test1 <- TrueScores + rnorm(26, mean = 0, sd = input$noise_sd)
    Test2 <- TrueScores + rnorm(26, mean = 0, sd = input$noise_sd)
    
    # Sort by Test1 and assign letters based on sorted order
    df <- data.frame(TrueScores = TrueScores, Test1 = Test1, Test2 = Test2) %>%
      arrange(desc(Test1)) %>%
      mutate(Subject = LETTERS[1:26]) %>%  # Assign letters in sorted order
      arrange(Subject)  # Reorder by Subject for display consistency
    
    df
  }
  
  # Initial score generation
  observe({
    scores$data <- generate_scores()
    scores$correlation <- cor(scores$data$Test1, scores$data$Test2)
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
  
  # Display scores in a table with colored letters for highest and lowest scores
  output$scoresTable <- renderTable({
    df <- scores$data
    
    # Identify the highest and lowest scores in Test1
    df <- df %>%
      mutate(Color = case_when(
        Test1 %in% tail(sort(Test1), 5) ~ "red",
        Test1 %in% head(sort(Test1), 5) ~ "green",
        TRUE ~ "black"
      ))
    
    # Apply HTML styling for colors in the table
    df %>% 
      mutate(Subject = sprintf("<span style='color:%s;'>%s</span>", Color, Subject)) %>%
      select(Subject, TrueScores, Test1, Test2)  # Drop Color column for display
  }, sanitize.text.function = identity)  # Allow HTML styling in the table
  
  # Plot box plots for Test1 and Test2 with colored and labeled points
  output$boxPlot <- renderPlot({
    # Prepare data with colors
    df <- scores$data %>%
      mutate(Color = case_when(
        Test1 %in% tail(sort(Test1), 5) ~ "red",
        Test1 %in% head(sort(Test1), 5) ~ "green",
        TRUE ~ "black"
      ))
    
    # Reshape data for box plot
    df_long <- df %>%
      pivot_longer(cols = c("Test1", "Test2"), names_to = "Test", values_to = "Score")
    
    # Create box plot
    ggplot(df_long, aes(x = Test, y = Score)) +
      geom_boxplot(outlier.shape = NA, fill = "gray80") +
      geom_text(aes(label = Subject, color = Color), position = position_jitter(width = 0.15, height = 0)) +
      scale_color_identity() +
      labs(
        title = paste("Correlation of Test1 and Test2, r =", round(scores$correlation, 2)),
        x = "Test", y = "Score"
      ) +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
