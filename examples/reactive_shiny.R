library(shiny)
library(async)
ui <- fluidPage(
  actionButton("minus", "set 10"),
  actionButton("plus", "set 20"),
  textOutput("value")
)

server <- function(input, output, session) {
  asy <- async$new(reactive = TRUE)

  observeEvent(input$minus, { asy$set(value=10) })
  observeEvent(input$plus,  { asy$set(value=20) })
  output$value <- renderText({ asy$status() })
}

shinyApp(ui, server)
