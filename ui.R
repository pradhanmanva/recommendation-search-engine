library(shiny)

fluidPage(
  
  titlePanel("Welcome to Recommendation Search Engine"),
  
  # Copy the line below to make a text input box
  textInput("text", label = h3("Enter the query"), value = ""),
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
)
