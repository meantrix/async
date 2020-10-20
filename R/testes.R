jsCode = glue("
  var {asyncheck} = true;

  SetInterval(function({input},{asyncheck}){
     if({asyncheck} == true):
        Shiny.addCustomMessageHandler(input , function(){
        Shiny.onInputChange(input,'_' + Math.random().toString(36).substr(2, 9));
    }, {timer},{input});
")


jsCode2 = glue("
     var {asyncheck} = false;
")


async$input = 'var2'


async_bar = function(async,session){

  activate = async$input
  shiny::observeEvent(input[[active]],{

  })


}

