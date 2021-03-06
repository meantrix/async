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
  asy2 = async$new(reactive = TRUE,auto.finish = FALSE,verbose = T)



  result <- future({
    print("Running...")
    for(i in 1:N){

      # Long Running Task
      Sys.sleep(2)

      # Notify status file of progress
      asy1$set(100*i/N, msg = 'test progress',detail='simple detail')
      asy2$inc(1, msg = 'test progress')

      # if(i > 70){
      #
      #   asy1$interrupt('close future...')
      #
      # }

    }

    #Some results
    quantile(rnorm(1000))
  })

 bar1 = asyncBar1$new(async=asy1,id='01',max.rep = 20,interval = 1000)
 bar2 = asyncBar1$new(async=asy2,id='02',max.rep = 20,interval = 1000)
 bar1$progress(session,input)
 bar2$progress(session,input)



}

# Run the application
shinyApp(ui = ui, server = server)
