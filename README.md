# Async

![logo](https://github.com/meantrix/async/blob/develop/inst/header_transparente_colorido.png)

<!--- These are examples. See https://shields.io for others or to customize this set of shields. You might want to include dependencies, project status and licence info here --->
[![version](https://img.shields.io/badge/version-0.0.6-green.svg)](https://semver.org)


async is a `tool` for passing messages between some R processes 
like shiny reactives and futures routines.
The great advantage of using this package is the fact that there is 
no copies creation of the shiny session in multiple workers to 
tracking asynchronous or parallel jobs.


## Prerequisites

Before you begin, ensure you have met the following requirements:
* We recommend that you have `R >= 3.5.0`
* You have a `Windows/Linux/Mac` machine.

## Installing async

To install `async`, follow these steps:


``` r
library(devtools)
install_github("meantrix/async")
```

## Using async example

Communicate between the app and the inside of the future:

```
library(shiny)
library(async)
library(future)
plan(multiprocess)

ui = fluidPage(
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

server = function(input, output) {
  N = 100
  asy = async$new(reactive = TRUE,auto.finish = FALSE)


    result = future({
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
```


## Contributing to async

To contribute to `async`, follow these steps:

1. Fork this repository.
2. Create a branch: `git checkout -b <branch_name>`.
3. Make your changes and commit them: `git commit -m '<commit_message>'`
4. Push to the original branch: `git push origin async/<location>`
5. Create the pull request.

Alternatively see the GitHub documentation on [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).

## Contributors

Thanks to the following people who have contributed to this project:

* [@meantrix](https://github.com/meantrix) 📖

## Contact

If you want to contact me you can reach me at <contato@meantrix.com>.

## License

This project uses the following license: [MIT Licence](<https://github.com/meantrix/async/blob/master/LICENSE>).




