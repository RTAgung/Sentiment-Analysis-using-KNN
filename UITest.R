library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(wordcloud2)
library(tidytext)
library(stringr)

ui <- fluidPage(
  title = "Analisis Sentimen Pengguna Twitter Terhadap Pandemi Covid19",
  headerPanel(""),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        h2("Analisis Sentimen Pengguna Twitter Terhadap Pandemi Covid19")
      ),
      br(),
      sliderInput(inputId = "n_data",
                  label = "Banyak Data",
                  value = 60,
                  min = 20,
                  max = 100
      ),
      actionButton("refresh_button", "Refresh"
      ),
      br(), br(),
      fluidRow(
        column(width = 12, "Rama Tri Agung / 123180053")
      ),
      fluidRow(
        column(width = 12, "Iffatuz Zahra / 123180039")
      )
    ),
    mainPanel(
      fluidRow(
        column(width = 6, plotlyOutput("d_plot", height = "220px", width = "400px")),
        column(width = 4, plotOutput("pie_plot", height = "210px", width = "390px"))
      ),
      fluidRow(
        tabsetPanel(type = "tab",
                    id = "tabset",
                    tabPanel("All Data", dataTableOutput("table_all")),
                    tabPanel("Cleaning Data", dataTableOutput("cleaning_data")),
                    tabPanel("Hasil Sentimen", dataTableOutput("sentiment")),
                    tabPanel("Wordcloud Negatif", wordcloud2Output("wordcloud_neg", height = "400px", width = "800px")),
                    tabPanel("wordcloud Positif", wordcloud2Output("wordcloud_pos", height = "400px", width = "800px"))
        )
      ),
      fluidRow(
        column(width = 6, h3("WordPlot Negatif")),
        column(width = 5, h3("WordPlot Positif"))
      ),
      fluidRow(
        column(width = 6, plotlyOutput("word_neg", height = "300px", width = "380px")),
        column(width = 5, plotlyOutput("word_pos", height = "300px", width = "380px")),
      ),
    )
  )
)

#server
server <- function(input, output, session) {
  observeEvent(input$refresh_button, {
    source("App.R")
  })

  d_view <- reactive({
    jumlah = input$n_data
    return(jumlah)
  })

  dp <- reactiveFileReader(1000, session, "data-raw/data_predict.csv", read.csv)
  result_predict <- reactiveFileReader(1000, session, "data-raw/data_predict_result.csv", read.csv)
  dp_clean <- reactiveFileReader(1000, session, "data-raw/data_predict_clean.csv", read.csv)

  d_plot <- reactive({
    result_predict_count <- result_predict() %>%
      head(d_view()) %>%
      count(sentiment)
    result_predict_count %>%
      ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
      geom_col() +
      geom_text(aes(label = n), color = "red") +
      labs(
        x = "",
        y = "Jumlah"
      ) +
      theme_light()
  })

  wc_positif <- reactive({
    dp_clean() %>%
      top_n(d_view()) %>%
      left_join(result_predict(), by = "id") %>%
      filter(sentiment == "Positive") %>%
      select(id, text.x, sentiment) %>%
      unnest_tokens(word, text.x) %>%
      count(word, sort = TRUE) %>%
      head(20)
  })

  wordplot_pos <- reactive({
    wc_positif() %>%
      head(10) %>%
      ggplot(aes(x = word, y = n, fill = word)) +
      geom_col() +
      geom_text(aes(label = n), color = "red") +
      labs(
        x = "",
        y = "Jumlah"
      ) +
      coord_flip() +
      theme_light()
  })

  wc_negatif <- reactive({
    dp_clean() %>%
      top_n(d_view()) %>%
      left_join(result_predict(), by = "id") %>%
      filter(sentiment == "Negative") %>%
      select(id, text.x, sentiment) %>%
      unnest_tokens(word, text.x) %>%
      count(word, sort = TRUE) %>%
      head(20)
  })

  wordplot_neg <- reactive({
    wc_negatif() %>%
      head(10) %>%
      ggplot(aes(x = word, y = n, fill = word)) +
      geom_col() +
      geom_text(aes(label = n), color = "red") +
      labs(
        x = "",
        y = "Jumlah"
      ) +
      coord_flip() +
      theme_light()
  })

  output$d_plot <- renderPlotly({
    ggplotly(d_plot())
  })

  output$pie_plot <- renderPlot({
    result_predict_count <- result_predict() %>%
      head(d_view()) %>%
      count(sentiment)
    prop <- round(result_predict_count$n * 100 / sum(result_predict_count$n), 1)
    ggplot(result_predict_count, aes(x = "", y = prop, fill = sentiment)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(y = cumsum(prop) - 0.99 * prop, label = paste0(prop, "%")), color = "yellow", size = 5) +
      labs(
        x = "",
        y = "persentase"
      )
  })


  output$table_all <- renderDataTable({
    dp() %>% head(d_view()) %>% select(-sentiment)
  })

  output$cleaning_data <- renderDataTable({
    dp_clean() %>% head(d_view())
  })

  output$sentiment <- renderDataTable({
    result_predict() %>% head(d_view())
  })

  output$wordcloud_neg <- renderWordcloud2({
    wordcloud2(wc_negatif(), size = 0.6)
  })

  output$wordcloud_pos <- renderWordcloud2({
    wordcloud2(wc_positif(), size = 0.6)
  })

  output$word_neg <- renderPlotly({
    ggplotly(wordplot_neg())
  })

  output$word_pos <- renderPlotly({
    ggplotly(wordplot_pos())
  })
}

#call shiny app
shinyApp(ui = ui, server = server, option = list(height = "500px"))
