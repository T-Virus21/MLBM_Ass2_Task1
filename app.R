# -------------------- #
#      LIBRARIES       #
# -------------------- #
library(shiny)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(DT)
data("stop_words")

# -------------------- #
#        UI            #
# -------------------- #
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Sentiment Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your CSV file:", accept = ".csv")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary",
                 h3("Dataset Overview"),
                 verbatimTextOutput("datasetInfo"),
                 br(),
                 h4("Data Preview"),
                 tableOutput("dataPreview")
        ),
        tabPanel("Plots",
                 h3("Histogram - Sentiment Score Distribution"),
                 plotOutput("sentimentHistogram"),
                 br(),
                 h3("Pie Chart - Sentiment Category Proportions"),
                 plotOutput("sentimentPieChart")
        ),
        tabPanel("Results",
                 tabsetPanel(
                   tabPanel("NPS Metrics",
                            h3("NPS Metrics"),
                            DTOutput("npsTable")
                   ),
                   tabPanel("CSAT Metrics",
                            h3("CSAT Metrics"),
                            DTOutput("csatTable")
                   ),
                   tabPanel("Feedback Metrics",
                            h3("Sentiment Categorization (Sample)"),
                            DTOutput("sentimentTable"),
                            br(),
                            h3("Top Keywords"),
                            DTOutput("topWordsTable")
                   )
                 )
        ),
        tabPanel("Interpretation",
                 h3("AI-Powered Business Insight"),
                 p("The analysis calculates two key metrics from the sentiment data:"),
                 tags$ul(
                   tags$li(strong("Net Promoter Score (NPS): "), 
                           "NPS = ((Number of Promoters - Number of Detractors) / Total Responses) * 100. 
                           This metric indicates customer loyalty and the likelihood of referrals. 
                           A high NPS suggests strong customer advocacy, while a low score signals the need for improvements in customer experience."),
                   tags$li(strong("Customer Satisfaction Score (CSAT): "), 
                           "CSAT = (Number of Promoters / Total Responses) * 100. 
                           This directly measures overall customer satisfaction. Higher CSAT values reflect a better customer experience.")
                 ),
                 p("In addition, sentiment analysis is performed on customer feedback using a lexicon-based approach. 
                   Each document's sentiment score is computed as the difference between the counts of positive and negative words. 
                   This provides a granular view of customer sentiment. Top keywords extracted from the text further highlight recurring themes in feedback, 
                   enabling targeted operational and strategic decisions.")
        )
      )
    )
  )
)

# -------------------- #
#       SERVER         #
# -------------------- #
server <- function(input, output) {
  
  # Reactive: Read CSV data
  tweets_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  # Data Summary outputs
  output$datasetInfo <- renderPrint({
    df <- tweets_data()
    cat("Number of rows:", nrow(df), "\n")
    cat("Number of columns:", ncol(df), "\n")
    cat("Column names:", paste(colnames(df), collapse = ", "), "\n")
  })
  
  output$dataPreview <- renderTable({
    head(tweets_data(), 10)
  })
  
  # Core Sentiment Analysis Calculation
  sentiment_result <- reactive({
    df <- tweets_data()
    validate(
      need("doc" %in% names(df), "Missing 'doc' column."),
      need("text" %in% names(df), "Missing 'text' column.")
    )
    df %>%
      select(doc, text) %>%
      unnest_tokens(word, text) %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(doc, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment_score = positive - negative)
  })
  
  # Plots Tab outputs
  output$sentimentHistogram <- renderPlot({
    req(sentiment_result())
    ggplot(sentiment_result(), aes(x = sentiment_score)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
      labs(title = "Sentiment Score Histogram",
           x = "Sentiment Score",
           y = "Frequency") +
      theme_minimal()
  })
  
  output$sentimentPieChart <- renderPlot({
    req(sentiment_result())
    sentiment_cat <- sentiment_result() %>%
      mutate(sentiment_category = case_when(
        sentiment_score > 0 ~ "Positive",
        sentiment_score < 0 ~ "Negative",
        TRUE ~ "Neutral"
      )) %>%
      count(sentiment_category) %>%
      mutate(percentage = round(100 * n / sum(n), 1))
    
    ggplot(sentiment_cat, aes(x = "", y = n, fill = sentiment_category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Sentiment Category Proportions") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  # Results Tab: Prepare Metrics Data
  metrics_data <- reactive({
    df <- sentiment_result()
    df <- df %>% mutate(nps_category = case_when(
      sentiment_score > 0 ~ "Promoter",
      sentiment_score < 0 ~ "Detractor",
      TRUE ~ "Passive"
    ))
    
    total <- nrow(df)
    promoters <- sum(df$nps_category == "Promoter")
    detractors <- sum(df$nps_category == "Detractor")
    passives <- sum(df$nps_category == "Passive")
    
    nps <- ((promoters - detractors) / total) * 100
    csat <- (promoters / total) * 100
    
    # Full metrics table
    data.frame(
      Metric = c("Total Tweets", "Promoters", "Passives", "Detractors", "NPS", "CSAT"),
      Value = c(total, promoters, passives, detractors, round(nps, 2), round(csat, 2))
    )
  })
  
  # NPS Tab
  output$npsTable <- renderDT({
    mdf <- metrics_data()
    nps_df <- mdf[mdf$Metric %in% c("Total Tweets", "Promoters", "Detractors", "NPS"), ]
    datatable(nps_df, options = list(pageLength = 4), class = "display")
  })
  
  # CSAT Tab
  output$csatTable <- renderDT({
    mdf <- metrics_data()
    csat_df <- mdf[mdf$Metric %in% c("Total Tweets", "Promoters", "CSAT"), ]
    datatable(csat_df, options = list(pageLength = 3), class = "display")
  })
  
  # Feedback Metrics Tab
  output$sentimentTable <- renderDT({
    datatable(head(sentiment_result(), 10),
              options = list(pageLength = 5),
              class = "display")
  })
  
  output$topWordsTable <- renderDT({
    tweets_data() %>%
      select(doc, text) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      head(10) %>%
      datatable(options = list(pageLength = 5), class = "display")
  })
}

# -------------------- #
#       RUN APP        #
# -------------------- #
shinyApp(ui = ui, server = server)
