library(shiny)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sentiment Analysis of Airline Tweets"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your CSV file:", accept = ".csv")
    ),
    
    mainPanel(
      plotOutput("sentimentPlot"),
      tableOutput("sentimentTable")
    )
  )
)

server <- function(input, output) {
  tweets_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  sentiment_result <- reactive({
    tweets_data() %>%
      select(doc, text) %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(doc, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment_score = positive - negative)
  })
  
  output$sentimentPlot <- renderPlot({
    sentiment_result() %>%
      mutate(sentiment_category = case_when(
        sentiment_score > 0 ~ "Positive",
        sentiment_score < 0 ~ "Negative",
        TRUE ~ "Neutral"
      )) %>%
      count(sentiment_category) %>%
      ggplot(aes(x = sentiment_category, y = n, fill = sentiment_category)) +
      geom_col() +
      labs(title = "Overall Sentiment Distribution", x = "Sentiment Category", y = "Number of Tweets") +
      theme_minimal()
  })
  
  output$sentimentTable <- renderTable({
    head(sentiment_result(), 10)
  })
}

shinyApp(ui = ui, server = server)
