library(shiny)
function(input, output) {
  
  # You can access the value of the widget with input$text, e.g.
  # output$value <- renderPrint({ input$text })
  
  output$value <- renderPrint({c(1,2,3,4,5)})
  
}