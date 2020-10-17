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
                      rxTrigger =  reactiveTrigger(),
                      .reactive =TRUE,

                      get_status = function(){
                        vm = scan(private$vars$status_file, what = "character",
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
                        private$get_status()[1] == "777"
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
                      #' @field  upper
                      upper = function (value) {
                        if (missing(value)) {
                          private$vars$upper
                        } else {
                          stop("`$upper` is read only", call. = FALSE)
                        }
                      },
                      lower = function (value) {
                        if (missing(value)) {
                          private$vars$lower
                        } else {
                          stop("`$lower` is read only", call. = FALSE)
                        }
                      },
                      auto.finish = function (value) {
                        if (missing(value)) {
                          private$vars$auto.finish
                        } else {
                          stop("`$auto.finish` is read only", call. = FALSE)
                        }
                      },
                      value_message = function (value) {
                        if (missing(value)) {
                          private$vars$vm
                        } else {
                          stop("`$value_message` is read only", call. = FALSE)
                        }
                      },
                      reactive = function (value) {
                        if (missing(value)) {
                          private$.reactive
                        } else {
                          stop("`$reactive` is read only", call. = FALSE)
                        }
                      }

                    ),


                    public = list(


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
                        private$rxTrigger =  reactiveTrigger()

                        private$vars$lower = lower
                        private$vars$upper = upper
                        private$vars$auto.finish = auto.finish

                      },

                      interrupt = function(msg="process interrupted"){

                        args = list(value = 777, msg = msg)
                        do.call(private$set_status, args = args)

                        if(isTRUE(private$.reactive)){
                          private$rxTrigger$trigger()
                        }
                      },

                      progress = function(value=0,msg="Running..."){
                        args = list(value = value, msg = msg)
                        checkmate::expect_numeric(value,lower = private$vars$lower,
                                                  upper = private$vars$upper)
                        checkmate::expect_character(msg,max.len = 1)

                        do.call(private$check_status,args = list())
                        do.call(private$set_status,args = args)

                        if(isTRUE(private$.reactive)){
                          private$rxTrigger$trigger()
                        }
                      },

                      status = function(){

                        if(isTRUE(private$.reactive)){
                          private$rxTrigger$depend()
                        }

                        do.call(private$get_status,args = list())
                        return(private$vars$vm)
                      },

                      check = function(){
                        do.call(private$check_status,args = list())
                      }



                    )


)
