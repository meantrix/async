library(shiny)
library(promises)
library(future)
library(async)
library(shinyWidgets)
plan(multiprocess)

ui <- tagList(shinyjs::useShinyjs(),
              fluidPage(titlePanel("Long Run Stoppable Async")))


server <- function(input, output, session) {

  N <- 5
  asy1 = async$new(
    reactive = TRUE,
    auto.finish = FALSE,
    lower = 0,
    upper = 100
  )
  bar = asyncBar2$new(
    async = asy1,
    id = '01',
    msg = 'fixed message',
    max.rep = 20,
    interval = 1000
  )
  bar$progress(session, input)
  bar$cancel(session, input, not.msg = "Aborting Process...")

  result <- future({
    print("Running...")
    for (i in 1:N) {
      # Long Running Task
      Sys.sleep(1)
      # Notify status file of progress
      asy1$set(100 * i / N, detail = paste0("...", as.character(100 * i /
                                                                  N), "..."))
    }
    # Some results
    quantile(rnorm(1000))
  })

  promises::then(result,
                 onFulfilled = function(res) {
                   showNotification("Future Finished", type = "message")
                   print(res)
                   bar$finalize()
                 },
                 onRejected = function(e) {
                   showNotification(e$message, type = "error")
                   print(e$message)
                   bar$finalize()
                 })

}

# Run the application
shinyApp(ui = ui, server = server)
