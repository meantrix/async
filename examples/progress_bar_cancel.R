library(shiny)
library(promises)
library(future)
library(async)
library(shinyWidgets)
library(reactlog)
plan(multiprocess)

ui <- tagList(
  shinyjs::useShinyjs(),
  fluidPage(
    titlePanel("Long Run Stoppable Async")
  )
)


server <- function(input, output,session) {
  N <- 100
  reactlog_enable()
  asy1 = async$new(reactive=TRUE,auto.finish=FALSE,lower = 0, upper = 100)
  asy2 = async$new(reactive = TRUE,auto.finish = FALSE)



  result <- future({
    print("Running...")
    for(i in 1:N){

      # Long Running Task
      Sys.sleep(2)

      # Notify status file of progress
      asy1$set(100*i/N,detail= paste0("...",as.character(100*i/N),"..."))


    }

    #Some results
    quantile(rnorm(1000))
  })

  bar2 = asyncBar2$new(async=asy1,id='01',msg = 'fixed message',max.rep = 20,interval = 1000)
  bar2$progress(session,input)
  bar2$cancel(session,input,not.msg="Aborting Process...")



}

# Run the application
shinyApp(ui = ui, server = server)
