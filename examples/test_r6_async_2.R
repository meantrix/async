ui <- fluidPage(
  actionButton("minus", "-1"),
  actionButton("plus", "+1"),
  textOutput("value")
)

server <- function(input, output, session) {
  asy <- async$new(reactive = TRUE)
  # scan(asy$status_file, what = "character",sep="\n", quiet = TRUE)
  # asy$progress(10)
  # scan(asy$status_file, what = "character",sep="\n", quiet = TRUE)
  # 
  
  observeEvent(input$minus, { asy$progress(value=10) })
  observeEvent(input$plus,  { asy$progress(value=20) })
  output$value <- renderText({ asy$status() })
}

shinyApp(ui, server)
