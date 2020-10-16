#'@title reactiveTrigger
#'@description A reactive trigger can be used when you want
#'to be able to explicitly trigger a reactive expression.
#'You can think of it as being similar to an action button,
#'except instead of clicking on a button to trigger an expression,
#'you can programatically cause the trigger.
#'This concept and code was created by Joe Cheng (author of shiny).

reactiveTrigger <- function() {
  async = shiny::reactiveVal(0)
  list(
    depend = function() {
      async()
      invisible()
    },
    trigger = function() {
      async( shiny::isolate(async()) + 1)
    }
  )
}



