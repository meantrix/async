## APP Example QUEUE

library(shiny)
library(future)
library(promises)
library(ipc)

# options(future.fork.enable = TRUE)
future::plan(list(future::multiprocess, future::sequential))
# on.exit(options(future.fork.enable = FALSE))

ui <- fluidPage(
  # textInput("projectName", "Project Name"),
  # numericInput("workers", "Workers", value = 3, min = 1),
  actionButton(inputId = "go", label = "Start Routine")#,
  #actionButton("interrupt", label = "Abort")
  #actionButton(inputId = "go2", label = "Load Project (future lapply)")
)

server <- function(input, output, session) {
  
  # A reactive value to hold failed files list
  failedFilesBA <- reactiveVal(NULL)
  
  # ** Ask for User Decision in case of Failed Files ----
  observeEvent(failedFilesBA(), {
    
    print("Attention Required. Asking to user whether to proceed or not.")
    
    showModal(
      modalDialog(
        title = h2("Attention Required", class = "h2-modal"),
        p("Some files could not be changed. If you proceed, a catastrophic failure will occur."),
        br(),
        wellPanel(
          div(
            tags$label(class = "control-label", strong("Failed Files")),
            br(),
            HTML(paste0('<span style="color: red;">', failedFilesBA(), '</span>', collapse = '<br>')),
            br(), br()
          ),
          style = "padding: 10px;"
        ),
        p("Are you sure you want to continue with the operation?"),
        footer = tagList(
          actionButton(class = "btn btn-success", inputId = "editAddBaseAreaAbort", label = "Abort"),
          actionButton(class = "btn btn-danger", inputId = "editAddBaseAreaContinue", label = "Proceed")
        ),
        size = "m", 
        easyClose = FALSE
      )
    )
    
  })
  
  # ** Handle User Response ----
  observeEvent(input$editAddBaseAreaAbort, {
    req(exists("queueBA2"))
    print("User chose to abort operation")
    # Send user message to the future
    queueBA2$producer$fireEval(shinyMessage <- "abort")
    removeModal()
  })
  observeEvent(input$editAddBaseAreaContinue, {
    req(exists("queueBA2"))
    print("User chose to remove failed files and continue")
    # Send user message to the future
    queueBA2$producer$fireEval(shinyMessage <- "continue")
    removeModal()
  })
  

  observeEvent(input$go, {
    
    # Start progress bar
    progress <- ipc::AsyncProgress$new(message = "Starting", value = 0.05)
    # Remove closeButton from progress notification
    shinyjs::runjs('$("div").remove(".shiny-notification-close");')
    
    # Queue to be consumed in Shiny Process (Send Signals from Future)
    queueBA <- ipc::shinyQueue()
    # Queue to be consumed in Future Process (Send Signals from Shiny)
    queueBA2 <<- ipc::queue() 
    # Double assignment to make this var available to other observers
    # Could be changed to a reactiveVar instead of double assignment
    
    # Start consumption of communication from the parent process
    # Execute signals every 250 milliseconds
    queueBA$consumer$start(250)
    
    progress$set(message = "Some important task", detail = "Starting Future")
    
    # Set variables
    globals <- list(
      progress = progress,
      queueBA = queueBA,
      queueBA2 = queueBA2
    )
    
    prom <- future::future(globals = globals, {
      
      progress$set(message = "Some important task", detail = "Running", value = 0.1)
      
      Sys.sleep(2)
      
      progress$set(message = "Some important task", detail = "Running", value = 0.3)
      
      Sys.sleep(2)
      
      progress$set(message = "Some important task", detail = "Running", value = 0.5)
      
      Sys.sleep(2)
      
      # Something gone wrong
      failed_files <- c("arquivo1", "arquivo2", "arquivo3")
      ### ASK USER FOR ACTION
      # Assign failed_files value to a reactive variable in the parent process (shiny)
      queueBA$producer$fireAssignReactive("failedFilesBA", failed_files)
      # Start consumption of communication from the parent process
      queueBA2$consumer$start(250) # From now on, will start listening to communication from the app
      
      # Wait until ShinyMessage appears
      while (!exists("shinyMessage")) 
        queueBA2$consumer$consume() # receive user order
      
      message("user choice: ", shinyMessage) 
      # shinyMessage var is defined outside of this future, and is received from queueBA2
      
      ### ABORT OR PROCEED WITH OPERATION
      ### IF USER WANTS TO CONTINUE, CONTINUE TASK, OTHERWISE, STOP FUTURE
      if (!shinyMessage == "continue") {
        stop("aborted")
      }
      
      Sys.sleep(1)
      
      progress$set(message = "Some important task", detail = "Continuing", value = 0.65)
      
      Sys.sleep(1)
      
      progress$set(message = "Some important task", detail = "Continuing", value = 0.80)
      
      Sys.sleep(1)
      
      progress$set(message = "Some important task", detail = "Continuing", value = 0.95)
      
      return()
      
    })
    
    promises::then(prom,
                   onFulfilled = function() {
                     showNotification("Task finish succesfully", type = "message", duration = 10)
                     failedFilesBA(NULL)
                     try(queueBA$destroy())
                     try(queueBA2$destroy())
                     progress$close()
                   },
                   onRejected = function(e) {
                     if (e$message == "aborted")
                       showNotification("Process aborted by user", type = "error", duration = 10)
                     failedFilesBA(NULL)
                     try(queueBA$destroy())
                     try(queueBA2$destroy())
                     progress$close()
                   })
    
    return(NULL) # End of observer
    
  })
  
}

shinyApp(ui = ui, server = server)