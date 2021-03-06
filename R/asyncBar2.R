#' @title  R6 Class asyncBar1
#' @description
#' Reports  async class progress with a custom shiny bar to end user.
#' @name asyncBar2
#' @export
NULL
asyncBar2 = R6::R6Class(classname = 'asyncBar2',
                        private = list(
                          async = NULL,
                          msg = NULL,
                          input = NULL,
                          id = NULL,
                          message = NULL,
                          notid = NULL,
                          funid = NULL,
                          cancelid = NULL,
                          counterid = NULL,
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
                                "find('.progress-bar').attr('style','width:' + width) })('" , id , "' , '", (100 * width) %>% round(.,digits = 2) %>% as.character(.) %>% paste0(.,"%") , "');"
                              )
                            )
                          },
                          progress_set = function(id, detail , width){
                            return(
                              paste0("((id, detail , width) => {",
                                     "$('#shiny-notification-' + id).find('.progress-detail').html(detail);",
                                     "$('#shiny-notification-' + id).find('.progress.progress-striped.active').find('.progress-bar').attr('style','width:' + width);",
                                     "})('" , id , "' , '" , detail , "' , '" , (100 * width) %>% round(.,digits = 2) %>% as.character(.) %>% paste0(.,"%") ,"'); "
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
                          progress_cancel = function(id,cancelid){
                            paste0("((id, cancelid) => {",
                                   "$('#shiny-notification-' + id)",
                                   ".find('.progress-message')",
                                   ".append(",
                                   "'<a id=\"' + cancelid + '\" class=\"action-button shiny-bound-input\"' +",
                                   "'style=\"font-size: 12px; cursor: pointer; font-weight: 500;\"> Cancel</a>'",
                                   ");",
                                   "})('" , id , "' , '" , cancelid ,"');"
                            )
                          },
                          interrupt_client = function(){
                            funid = private$funid
                            jsCode = paste0("clearInterval(window.",funid,");")
                            shinyjs::runjs(jsCode)
                            shinyjs::runjs(private$progress_close(private$id))
                            #unlink(private$async$status_file)
                          },
                          create_progress  = function(){
                            vars = private$async$.__enclos_env__$private$get_status()
                            detail = as.character(vars[3])
                            msg = private$msg
                            val = as.numeric(vars[1])
                            funid = private$funid
                            input = private$input
                            timer = private$interval
                            jsCode = paste0(funid," = setInterval(function(){",
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
                              msg = private$msg
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
                                #shiny::removeNotification(private$notid)
                                private$interrupt_client()

                              } else {
                                width = (val - min.val)/(max.val-min.val)
                                shinyjs::runjs(private$progress_set(id = private$id ,
                                                                    detail = detail,
                                                                    width = width))
                              }
                            },domain = session)
                          },

                          create_cancel = function(){

                            id = private$id
                            cancelid = private$cancelid
                            counterid = private$counterid


                            shinyjs::runjs(private$progress_cancel(id = id,cancelid = cancelid))

                            jsCode = paste0(
                            "var ",counterid," = 0;",
                            "$('#",cancelid,"').on('click', function(){",
                              counterid,"++;",
                           "Shiny.onInputChange('",cancelid,"',",counterid,");",
                          "});")

                            shinyjs::runjs(jsCode)


                          },
                          observe_event_cancel = function(session,input,not.c,not.msg){
                              shiny::observeEvent(input[[private$cancelid]],{

                              if(isTRUE(not.c)){
                                notid = private$notid
                                shiny::showNotification(not.msg,
                                                        type = "warning",
                                                        id = notid,
                                                        duration = NULL,
                                                        closeButton = FALSE)
                              }
                              private$async$interrupt()

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
                          #' @param msg A single-element character vector;
                          #'  the message to be displayed to the user,
                          initialize = function(async,
                                                id,
                                                interval=1000,
                                                max.rep = 50,
                                                msg){


                            if(missing(msg)){
                              msg = ""
                            }

                            checkmate::expect_class(async,'R6')
                            checkmate::expect_character(id,max.len = 1)
                            checkmate::expect_character(msg,max.len = 1)
                            checkmate::expect_numeric(interval,lower = 0,upper = Inf)
                            checkmate::expect_numeric(max.rep,lower = 1,upper = Inf)

                            vars.id = do.call(paste0, Map(stringi::stri_rand_strings, n=5, length=c(5, 4, 2),
                                                          pattern = c('[A-Z]', '[0-9]', '[A-Z]')))
                            private$input = vars.id[1]
                            private$funid = vars.id[2]
                            private$cancelid = vars.id[3]
                            private$counterid = vars.id[4]
                            private$notid = vars.id[5]
                            private$id = id
                            private$max.rep = max.rep
                            private$interval = interval
                            private$async = async


                            last.val  = as.numeric(async$.__enclos_env__$private$get_status()[1])
                            private$msg = msg
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
                          #' display  a cancel button to end user
                          #' associated with the process tracked by the async class
                          #' @param session shiny session
                          #' @param input shiny input
                          #' @param not.msg Content of notification message
                          cancel = function(session,input,not.msg){

                            not.c = FALSE

                            if(!missing(not.msg)){
                              not.c = checkmate::check_character(not.msg,max.len = 1)
                            }

                            if(missing(session)){
                              session = shiny::getDefaultReactiveDomain()
                            }

                            private$create_cancel()
                            private$observe_event_cancel(session,input,not.c = not.c,
                                                         not.msg = not.msg)


                          },
                          #' @description
                          #' Close all routines of async bar.
                          finalize = function(){
                            private$interrupt_client()
                            shiny::removeNotification(private$notid)
                            private$async$finalize()
                          }
                        )
)
