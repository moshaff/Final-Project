library(shiny)
library(tidyverse)

Netflix <- read.csv("Netflix_Tweet_sentiment.csv", stringsAsFactors = FALSE) # Read in Netflix sentiment data

ui <- fluidPage( 
  titlePanel("Twitter Sentiment for Tweets About Netflix"),  # Title
  sidebarLayout(
    sidebarPanel(sliderInput("scoreInput", "Sentiment Score", min = -5, max = 5, # Establish input and input min/max
                             value = c(-1, 1))), # Starting slider positions
    mainPanel( plotOutput("coolplot"),
               br(),br(),
               tableOutput("results"))
  )
)

server <- function(input, output) {
  output$coolplot <- renderPlot({
    filtered <-
      Netflix %>%
      filter(score >= input$scoreInput[1], # Filter out all the results greater than the max
             score <= input$scoreInput[2]  # Filter out all the results less than the min
      )
    
    ggplot(filtered, aes(score)) +
      geom_histogram()              # Create a histogram for sentiment score and count
  }) 
  
  output$results <- renderTable({
    filtered <-
      Netflix %>%
      filter(score >= input$scoreInput[1], # Filter out all the results greater than the max
             score <= input$scoreInput[2]  # Filter out all the results less than the min
      )
    filtered
  })
  
}
shinyApp(ui = ui, server = server) # Set up the shiny app

