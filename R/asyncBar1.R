#' @title  R6 Class asyncBar1
#' @description
#' Reports  async class progress with a default shiny bar to end user.
#' @name asyncBar1
#' @export
NULL
asyncBar1 = R6::R6Class(classname = 'asyncBar1',
                        private = list(
                          async = NULL,
                          input = NULL,
                          id = NULL,
                          fun.id = NULL,
                          interval = NULL,
                          last.val = NULL,
                          max.rep = NULL,
                          n.rep = 0,
                          progress_new = function(id , message , detail , show=TRUE){
                            return(
                              paste0(
                                "((id , message , detail , show) => {",
                                "if($('#shiny-notification-panel').length == 0) $('body').append('<div id=\"shiny-notification-panel\"></div>'); ",
                                "$('#shiny-notification-panel').append(",
                                "'<div id=\"shiny-notification-' + id + '\" class=\"shiny-notification\" style=\"display: none;\">' +",
                                "'<div class=\"shiny-notification-close\"> &times; </div>' +",
                                "'<div class=\"shiny-notification-content\">' +",
                                "'<div class=\"shiny-notification-content-text\">' +",
                                "'<div id=\"shiny-progress-' +  id  + '\" class=\"shiny-progress-notification\">' +",
                                "'<div class=\"progress progress-striped active\" style=\"display: none;\">' +",
                                "'<div class=\"progress-bar\" style=\"width: 0%;\"></div>' +",
                                "'</div>' +",
                                "'<div class=\"progress-text\">' +",
                                "'<span class=\"progress-message\">' + message + '</span>' +",
                                "'<br>' +",
                                "'<span class=\"progress-detail\">' + detail + '</span>' + ",
                                "'</div>' +",
                                "'</div>' +",
                                "'</div>' +",
                                "'<div class=\"shiny-notification-content-action\"></div>' + ",
                                "'</div>' +",
                                "'</div>'",
                                ");",
                                "$('#shiny-notification-' + id ).find('.shiny-notification-close').on('click' , () =>{",
                                "$('#shiny-notification-' + id ).hide(); ",
                                "}); ",
                                "if(show) $('#shiny-notification-' + id ).show();" ,
                                "})('" , id , "' , '" , message , "' , '" , detail , "' , " , show %>% as.character(.) %>% tolower(.) ,");"
                              )
                            )
                          },
                          progress_inc = function(id , width){
                            return(
                              paste0(
                                "((id , width) => { $('#shiny-notification-' + id).",
                                "find('.progress.progress-striped.active').",
                                "find('.progress-bar').attr('style','width:' + width) })('" , id , "' , '", (100 * width) %>% round(.,digits = 0) %>% as.character(.) %>% paste0(.,"%") , "');"
                              )
                            )
                          },
                          progress_set = function(id , message , detail , width){
                            return(
                              paste0("((id , message , detail , width) => {",
                                     "$('#shiny-notification-' + id).find('.progress-message').html(message);"	,
                                     "$('#shiny-notification-' + id).find('.progress-detail').html(detail);",
                                     "$('#shiny-notification-' + id).find('.progress.progress-striped.active').find('.progress-bar').attr('style','width:' + width);",
                                     "})('" , id , "' , '" , message , "' ,  '" , detail , "' , '" , (100 * width) %>% round(.,digits = 1) %>% as.character(.) %>% paste0(.,"%") ,"'); "
                              )
                            )
                          },
                          progress_showbar = function(id){
                            return(paste0("((id) => { $('#shiny-notification-' + id).find('.progress.progress-striped.active').show(); })('" , id , "'); "))
                          },
                          progress_hidebar = function(id) {
                            return(paste0("((id) => { $('#shiny-notification-' + id).find('.progress.progress-striped.active').hide(); })('" , id , "'); "))
                          },
                          progress_close = function(id){
                            return(paste0("$('#shiny-notification-" , id , "').remove(); "))
                          },
                          interrupt_client = function(){
                            fun.id = private$fun.id
                            jsCode = paste0("clearInterval(window.",fun.id,");")
                            shinyjs::runjs(jsCode)
                            shinyjs::runjs(private$progress_close(private$id))
                            #unlink(private$async$status_file)
                          },
                          create_progress  = function(){
                            #browser()
                            vars = private$async$.__enclos_env__$private$get_status()
                            detail = as.character(vars[3])
                            msg = as.character(vars[2])
                            val = as.numeric(vars[1])
                            fun.id = private$fun.id
                            input = private$input
                            timer = private$interval
                            jsCode = paste0(fun.id," = setInterval(function(){",
                                            "Shiny.onInputChange('",input,"','_' + Math.random().toString(36).substr(2, 9));",
                                            "},",timer,");")
                            shinyjs::runjs(jsCode)
                            shinyjs::runjs(private$progress_new(id = private$id,
                                                                detail = detail,
                                                                message = msg))
                            shinyjs::runjs(private$progress_showbar(id = private$id))
                          },
                          observe_event = function(session,input){
                            shiny::observeEvent(input[[private$input]],{
                              #browser()
                              vars = private$async$status()
                              detail = as.character(vars[3])
                              msg = as.character(vars[2])
                              val = as.numeric(vars[1])
                              max.val = private$async$upper
                              min.val = private$async$lower
                              val = ifelse(is.numeric(val),val,private$last.val)
                              msg = ifelse(is.character(msg),msg,'')
                              detail = ifelse(is.character(detail),detail,'')
                              if( isTRUE(val == private$last.val) ){
                                private$n.rep = private$n.rep + 1
                              } else {
                                private$n.rep = 0
                              }
                              private$last.val = val
                              if( isTRUE(val == 7777) |
                                  isTRUE(val == max.val) |
                                  isTRUE(private$n.rep > private$max.rep) ) {
                                private$interrupt_client()
                              } else {
                                width = (val - min.val)/(max.val-min.val)
                                shinyjs::runjs(private$progress_set(id = private$id ,
                                                                    message = msg ,
                                                                    detail = detail,
                                                                    width = width))
                              }
                            },domain = session)
                          }
                        ),
                        public = list(
                          #' @description
                          #' Interactive environment
                          #' to create a progress  bar of async tracking.
                          #' @param async R6 class. Object to tracking routines.
                          #' @param id character. ID of progress bar.
                          #' @param interval numeric. Approximate number of milliseconds
                          #' to wait between checks of the job progress.
                          #' @param max.rep numeric. Maximum number of times that
                          #' a progress value can be repeated.
                          initialize = function(async,
                                                id,
                                                interval=1000,
                                                max.rep = 50){
                            checkmate::expect_class(async,'R6')
                            checkmate::expect_character(id,max.len = 1)
                            checkmate::expect_character(detail,max.len = 1)
                            checkmate::expect_numeric(interval,lower = 0,upper = Inf)
                            checkmate::expect_numeric(max.rep,lower = 1,upper = Inf)
                            vars.id = do.call(paste0, Map(stringi::stri_rand_strings, n=2, length=c(5, 4, 1),
                                                          pattern = c('[A-Z]', '[0-9]', '[A-Z]')))
                            private$input = vars.id[1]
                            private$fun.id = vars.id[2]
                            private$id = id
                            private$max.rep = max.rep
                            private$interval = interval
                            private$async = async
                            private$detail = detail
                            vars.status = as.numeric(async$.__enclos_env__$private$get_status())
                            last.val  = vars.status[1]
                            private$last.val = ifelse(is.numeric(last.val),last.val,async$lower)
                            private$create_progress()
                          },
                          #' @description
                          #' observe event to checks of the async job progress.
                          #' @param session shiny session
                          #' @param input shiny input
                          progress = function(session,input) {
                            if(missing(session)){
                              session = shiny::getDefaultReactiveDomain()
                            }
                            private$observe_event(session,input)
                          },
                          #' @description
                          #' Close all routines of async bar.
                          finalize = function(){
                            private$interrupt_client()
                          }
                        )
)
