library(shiny)
library(promises)
library(future)
library(async)
library(shinyWidgets)
plan(multiprocess)

ui <- tagList(
  shinyjs::useShinyjs(),
  fluidPage(
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
)

server <- function(input, output,session) {
  N <- 100
  asy1 = async$new(reactive = TRUE,auto.finish = FALSE)
  #asy2 = async$new(reactive = TRUE,auto.finish = FALSE)



  result <- future({
    print("Running...")
    for(i in 1:N){

      # Long Running Task
      Sys.sleep(1)

      # Notify status file of progress
      asy1$progress(100*i/N, msg = 'test progress')
      #asy2$progress(100*i/N, msg = 'test progress')

      if(i > 90){

        asy1$interrupt('close future...')

      }

    }

    #Some results
    quantile(rnorm(1000))
  })

 bar1 = async_bar$new(async=asy1,id='01')
 #bar2 = async_bar$new(async=asy2,session=session,id='02')
 bar1$progress(session,input)
 #bar2$progress(session,input)


 #browser()


}

# Run the application
shinyApp(ui = ui, server = server)
