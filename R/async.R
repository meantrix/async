#'@title reactiveTrigger
#'@description A reactive trigger can be used when you want
#'to be able to explicitly trigger a reactive expression.
#'You can think of it as being similar to an action button,
#'except instead of clicking on a button to trigger an expression,
#'you can programatically cause the trigger.
#'This concept and code was created by Joe Cheng (author of shiny).

reactiveTrigger = function() {
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






#' @title  R6 Class async
#' @description
#' simple tool to pass message of progress and interruption of routines
#' between some R processes like shiny reactives and futures routines.
#' @name async
#' @export
NULL

async = R6::R6Class(classname = 'async',

                    private = list(

                      vars = list(status_file = NULL,
                                  upper = 100,
                                  lower = 0,
                                  auto.finish = TRUE,
                                  vm = NULL
                      ),
                      rx_trigger =  reactiveTrigger(),
                      .reactive =TRUE,

                      get_status = function(){
                        vm = scan(private$vars$status_file,
                                  what = "character",
                                  sep="\n",quiet = TRUE)
                        vm = stringr::str_split(vm,' zzzz ')[[1]]
                        names(vm) = c('value','message')
                        private$vars$vm = vm
                        vm
                      },

                      set_status = function(value, msg = ""){

                        vm = paste0(value,' zzzz ',msg)
                        write(vm, private$vars$status_file)

                        vm.out = stringr::str_split(vm,' zzzz ')[[1]]
                        names(vm.out) = c('value','message')
                        private$vars$vm = vm.out

                      },

                      interrupted = function(){
                        private$get_status()[1] == "7777"
                      },


                      check_status = function(){

                        if( private$interrupted() ){
                          #unlink( private$vars$status_file)
                          stop("User Interrupt")

                        }

                        if(private$get_status()[1] == as.character(private$vars$upper)
                           && private$vars$auto.finish) {
                          #unlink(private$vars$status_file)
                          stop("Process finished")
                        }
                      }
                    ),

                    active = list(
                      #' @field  status_file path to temporary file
                      status_file = function (value) {
                        if (missing(value)) {
                          private$vars$status_file
                        } else {
                          stop("`$status_file` is read only", call. = FALSE)
                        }
                      },
                      #'@field  upper The value that represents the end of the routine progress.
                      upper = function (value) {
                        if (missing(value)) {
                          private$vars$upper
                        } else {
                          stop("`$upper` is read only", call. = FALSE)
                        }
                      },
                      #'@field lower The value that represents the starting
                      #'point of the routine progress.
                      lower = function (value) {
                        if (missing(value)) {
                          private$vars$lower
                        } else {
                          stop("`$lower` is read only", call. = FALSE)
                        }
                      },
                      #'@field auto.finish if it is true the routine will
                      #'be interrupted when the progress is equal to the upper value.
                      auto.finish = function (value) {
                        if (missing(value)) {
                          private$vars$auto.finish
                        } else {
                          stop("`$auto.finish` is read only", call. = FALSE)
                        }
                      },
                      #'@field value_message get job status.
                      value_message = function (value) {
                        if (missing(value)) {
                          private$vars$vm
                        } else {
                          stop("`$value_message` is read only", call. = FALSE)
                        }
                      },
                      #'@field reactive If its true generate a reactive expression
                      #' is a expression whose result will change over time.
                      reactive = function (value) {
                        if (missing(value)) {
                          private$.reactive
                        } else {
                          stop("`$reactive` is read only", call. = FALSE)
                        }
                      }

                    ),


                    public = list(

                      #' @description
                      #' create an interactive environment
                      #' to pass progress message between
                      #' R processes.
                      #' @param lower The value that represents the starting
                      #'point of the routine progress.
                      #' @param upper upper The value that represents
                      #' the end of the routine progress.
                      #' @param auto.finish auto.finish if it is true the routine will
                      #'be interrupted when the progress is equal to the upper value.
                      #' @param reactive If its true generate a reactive expression
                      #' is a expression whose result will change over time.
                      initialize = function(lower=0, upper=100,
                                            auto.finish = TRUE,
                                            reactive=TRUE){

                        checkmate::expect_numeric(c(lower,upper),lower = 0,upper = 100)
                        checkmate::expect_logical(auto.finish,max.len = 1)
                        checkmate::expect_logical(reactive,max.len = 1)

                        status_file = tempfile()
                        private$vars$status_file = status_file
                        #init.msg = paste0(0,' zzzz ','init file')
                        # write(init.msg, status_file)
                        vm = private$vars$vm
                        write(paste0(vm$value,' zzzz ',vm$msg), private$vars$status_file)

                        private$.reactive = reactive
                        private$rx_trigger =  reactiveTrigger()

                        private$vars$lower = lower
                        private$vars$upper = upper
                        private$vars$auto.finish = auto.finish

                      },

                      #' @description
                      #' Interrupt checking to future processes.
                      #' @param msg A single-element character vector;
                      #' the message to be displayed to the user.
                      interrupt = function(msg="process interrupted"){

                        args = list(value = 7777, msg = msg)
                        do.call(private$set_status, args = args)

                        if(isTRUE(private$.reactive)){
                          private$rx_trigger$trigger()
                        }
                      },
                      #' @description
                      #' Set progress to future processes.
                      #' @param value  the value at which to set the progress,
                      #' relative to lower and upper.
                      #' @param msg A single-element character vector;
                      #' the message to be displayed to the user.
                      progress = function(value=0,msg="Running..."){
                        args = list(value = value, msg = msg)
                        checkmate::expect_numeric(value,lower = private$vars$lower,
                                                  upper = private$vars$upper)
                        checkmate::expect_character(msg,max.len = 1)

                        do.call(private$check_status,args = list())
                        do.call(private$set_status,args = args)

                        if(isTRUE(private$.reactive)){
                          private$rx_trigger$trigger()
                        }
                      },
                      #' @description
                      #' get the status of the process out
                      #' of the future context
                      status = function(){

                        if(isTRUE(private$.reactive)){
                          private$rx_trigger$depend()
                        }

                        do.call(private$get_status,args = list())
                        return(private$vars$vm)
                      },
                      #' @description
                      #' Check the status of the process in
                      #' the future context
                      check = function(){
                        do.call(private$check_status,args = list())
                      },
                      #' @description
                      #' Close all routines of async tracking.
                      #' @param session shiny session
                      finalize = function(){
                      unlink( private$vars$status_file)

                      }



                    )


)
