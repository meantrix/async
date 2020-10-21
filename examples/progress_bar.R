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
      actionButton('status', 'Check Status')
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
      asy$progress(100*i/N, msg = 'test progress')

    }

    #Some results
    quantile(rnorm(1000))
  })

 bar = async_bar$new(async=asy,session=session,id='01')

 bar$run(session,input)

 browser()

}

# Run the application
shinyApp(ui = ui, server = server)
