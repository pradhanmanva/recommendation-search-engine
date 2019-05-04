library(shiny)

#UI Logic
ui <- fluidPage(
  titlePanel("Recommendation Search Engine"),
  sidebarLayout(
    sidebarPanel(
      helpText("Type in query to see the movie plots that match best."),
      textInput("query", h3("Text input", align = "center"), value = "Query")),
    mainPanel(
      h2("Let's see how this engine works.", align = "left"),
      h4("Let's see how it works"),
      p("We have to put in the query (single or multiword) that results in creating a Vector of Documents. Then a Document-Term/ Term-Document Matrix is generated with respect to the query and normalized weighted Term Frequency - Inverse Document Frequency (TD-IDF)."),
      p("Then this DTM/TDM is converted to dataframe (tibble) for further processing. We strip the ", em("dataframe$terms"), " to lower alphabets. Then filter out the ", em("dataframe$terms"), " and get only relevant ones."),
      p("In case of single word query, only top ten documents (plot summaries) with highest TD-IDF are returned in the results, while in multi word query, the cosine similiarity between query and documents is calculated. Top ten similiar documents are returned in the results."),
      h3("Here are the results: "),
      textOutput("results")
    )
  )
)

#Server logic
server <- function(input, output) {
  output$results <- renderText(input$query)
}

# Run the app ----
shinyApp(ui = ui, server = server)