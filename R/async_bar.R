



async = R6::R6Class(classname = 'async_bar',

  private = list(

      private$input = NULL,
      private$asyncheck = NULL,
      private$id = NULL,
      private$async = NULL,
      private$detail = NULL

      #' @description : Create new progress bar.
      #' @title progress_new
      #' @param id      : ID of progress bar.
      #' @param message : Main message to display.
      #' @param detail  : detail message to display.
      #' @param show    : TRUE/FALSE.
      #' @return : js command.
      progress_new = function(id , message , detail , show){
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
      #' @description : Progress Bar Inc.
      #' @title progress_inc
      #' @param id      : ID of progress bar.
      #' @param width   : width value.
      #' @return : js command.
      #' @export
      #'
      progress_inc = function(id , width){
        return(
          paste0(
            "((id , width) => { $('#shiny-notification-' + id).",
            "find('.progress.progress-striped.active').",
            "find('.progress-bar').attr('style','width:' + width) })('" , id , "' , '", (100 * width) %>% round(.) %>% as.character(.) %>% paste0(.,"%") , "');"
          )
        )
      }
      #' @description : Set message to progress bar.
      #' @title progress_set
      #' @param id      : ID of progress bar.
      #' @param message : Main message to display.
      #' @param detail  : detail message to display.
      #' @param width   : value for width bar.
      #' @return : js command.
      #'
      progress_set = function(id , message , detail , width){
        return(
          paste0("((id , message , detail , width) => {",
                 "$('#shiny-notification-' + id).find('.progress-message').html(message);"	,
                 "$('#shiny-notification-' + id).find('.progress-detail').html(detail);",
                 "$('#shiny-notification-' + id).find('.progress.progress-striped.active').find('.progress-bar').attr('style','width:' + width);",
                 "})('" , id , "' , '" , message , "' ,  '" , detail , "' , '" , (100 * width) %>% round(.) %>% as.character(.) %>% paste0(.,"%") ,"'); "
          )
        )
      }
      #' @description : Show progres bar.
      #' @title progress_showbar
      #' @param id      : ID of progress bar.
      #' @return : js command.
      progress_showbar = function(id){
        return(paste0("((id) => { $('#shiny-notification-' + id).find('.progress.progress-striped.active').show(); })('" , id , "'); "))
      }
      #' @description : Hide progress bar.
      #' @title progress.hide
      #' @param id      : ID of progress bar.
      #' @return : js command.
      progress_hidebar = function(id) {
        return(paste0("((id) => { $('#shiny-notification-' + id).find('.progress.progress-striped.active').hide(); })('" , id , "'); "))
      }

      #' @description : Remove progress bar.
      #' @title progress.close
      #' @param id      : ID of progress bar.
      #' @return : js command.

      progress.close = function(id){
        return(paste0("$('#shiny-notification-" , id , "').remove(); "))
      },

      #' @description : Interrupt progress update in client side.
      #' @title interrupt_client
      #' @param session : shiny session.
      interrupt_client = function(session){

        jsCode = glue::glue("var {asyncheck} = false;",asyncheck = private$asyncheck)
        shinyjs::runjs(jsCode)
        private$progress.close(private$id)

      },

      create_progress  = function(session){

        jsCode = glue("
        var {asyncheck} = true;

        SetInterval(function({input},{asyncheck}){
          if({asyncheck} == true):
            Shiny.addCustomMessageHandler(input , function(){
            Shiny.onInputChange({input},'_' + Math.random().toString(36).substr(2, 9));
        }), {timer},{input});",asyncheck = private$async,input=private$input,timer = private$interval)

        shinyjs::runjs(jsCode)
        private$progress_new(id = private$id,detail = private$detail,message = private$msg)


      },

      observeEvent = function(session){

        shiny::observeEvent(input[private$input],{

          vars = private$async$status()[1]
          value = vars[1]
          msg = vars[2]
          private$progress_set(id = private$id ,
                               message = msg ,
                               detail = private$detail,
                               width = value)

        })


      }



  ),

  public = list(

      initialize = function(async ,id,interval=400,detail,session){


        checkmate::expect_class(async,'R6')
        checkmate::expect_character(id,max.len = 1)
        checkmate::expect_character(detail,max.len = 1)
        checkmate::expect_numeric(interval,lower = 0,upper = Inf)
        if(missing(session)){
        session = shiny::getDefaultReactiveDomain()
        }

      input = do.call(paste0, Map(stringi::stri_rand_strings, n=1, length=c(5, 4, 1),
                                          pattern = c('[A-Z]', '[0-9]', '[A-Z]')))

      private$input = input
      private$id = id
      private$interval = interval
      private$asyncheck = 'true'
      private$async = async

    }




  )

)
