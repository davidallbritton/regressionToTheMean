library(shiny)
library(dplyr)

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
      plotOutput("scorePlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store the scores
  scores <- reactiveValues(TrueScores = NULL, Test1 = NULL, Test2 = NULL, Subject = NULL)
  
  # Generate TrueScores and initial Test scores on app load
  observe({
    set.seed(123)  # For reproducibility
    scores$TrueScores <- rnorm(26, mean = 100, sd = 15)
    scores$Test1 <- scores$TrueScores + rnorm(26, mean = 0, sd = input$noise_sd)
    scores$Test2 <- scores$TrueScores + rnorm(26, mean = 0, sd = input$noise_sd)
    scores$Subject <- LETTERS[1:26]  # A to Z for 26 subjects
  })
  
  # Regenerate Test1 when the button is pressed
  observeEvent(input$regenerate_test1, {
    scores$Test1 <- scores$TrueScores + rnorm(26, mean = 0, sd = input$noise_sd)
  })
  
  # Regenerate Test2 when the button is pressed
  observeEvent(input$regenerate_test2, {
    scores$Test2 <- scores$TrueScores + rnorm(26, mean = 0, sd = input$noise_sd)
  })
  
  # Display scores in a table with colored letters for highest and lowest scores
  output$scoresTable <- renderTable({
    df <- data.frame(Subject = scores$Subject,
                     TrueScores = scores$TrueScores, 
                     Test1 = scores$Test1, 
                     Test2 = scores$Test2)
    
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
      select(-Color)  # Drop the Color column for display
  }, sanitize.text.function = identity)  # Allow HTML styling in the table
  
  # Plot Test1 vs Test2 with subject labels and colored letters
  output$scorePlot <- renderPlot({
    # Prepare data with colors
    df <- data.frame(Subject = scores$Subject,
                     TrueScores = scores$TrueScores, 
                     Test1 = scores$Test1, 
                     Test2 = scores$Test2) %>%
      mutate(Color = case_when(
        Test1 %in% tail(sort(Test1), 5) ~ "red",
        Test1 %in% head(sort(Test1), 5) ~ "green",
        TRUE ~ "black"
      ))
    
    # Plot Test1 vs Test2 with colored labels
    plot(df$Test1, df$Test2, type = "n",
         xlab = "Test1", ylab = "Test2",
         main = "Test1 vs Test2")
    text(df$Test1, df$Test2, labels = df$Subject, col = df$Color)  # Add subject labels with colors
    abline(a = 0, b = 1, col = "black", lty = 2)  # Reference line
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
