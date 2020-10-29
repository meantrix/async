library(shiny)
library(promises)
library(future)
library(async)
library(shinyWidgets)
plan(multiprocess)

ui <- fluidPage(
  titlePanel("Long Run Stoppable Async"),
  sidebarLayout(
    sidebarPanel(
      actionButton('cancel', 'Cancel'),
      actionButton('status', 'Check Status'),
      progressBar(
            id = "pb2",
            value = 0,
            total = 100,
            title = "",
            display_pct = TRUE
          )

    ),
    mainPanel(
      tableOutput("result")
    )
  )
)

server <- function(input, output,session) {
  N <- 100
  asy = async$new(reactive = TRUE,auto.finish = FALSE)


    result <- future({
      print("Running...")
      for(i in 1:N){

        # Long Running Task
        Sys.sleep(1)

        # Notify status file of progress
        asy$inc(100*i/N, msg = 'test progress')

      }

      #Some results
      quantile(rnorm(1000))
    })


  # Register user interrupt
  observeEvent(input$cancel,{
    print("Cancel")
    asy$interrupt()
  })

  #Let user get analysis progress
  observeEvent(input$status,{
    print("Status")
    print(asy$status())
    updateProgressBar(
              session = session,
              id = "pb2",
              value = asy$status()[[1]], total = 100,
              title = 'test progress bar'
            )
  })



}

# Run the application
shinyApp(ui = ui, server = server)
