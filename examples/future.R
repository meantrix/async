library(shiny)
library(promises)
library(future)
plan(multiprocess)

ui <- fluidPage(
  titlePanel("Long Run Stoppable Async"),
  sidebarLayout(
    sidebarPanel(
      actionButton('cancel', 'Cancel'),
      actionButton('status', 'Check Status')
    ),
    mainPanel(
      tableOutput("result")
    )
  )
)

server <- function(input, output) {
  N <- 100
  asy = async$new(reactive = TRUE,auto.finish = FALSE)


    result <- future({
      print("Running...")
      for(i in 1:N){

        # Long Running Task
        Sys.sleep(1)

        # Notify status file of progress
        asy$progress(100*i/N)

      }

      #Some results
      quantile(rnorm(1000))
    })


  # Register user interrupt
  observeEvent(input$cancel,{
    print("Cancel")
    asy$interrupt()
  })

  # Let user get analysis progress
  observeEvent(input$status,{
    print("Status")
    print(asy$status())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
