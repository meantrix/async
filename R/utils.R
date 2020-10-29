#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @title save_async
#' @description Functions to write a single async object to a  RDS file.
#' @param async R6 class. Object to tracking routines.
#' @param file a connection or the name of the file where the R object is saved to or read from.
#' If its is missing a unique name file is created by default.
#' @export
save_async = function(async, file){

  if(missing(file)){

    file = file.path(tempdir(),paste0(uuid::UUIDgenerate(),".rds" ))
    message('unique path created:',file)

  }

  checkmate::expect_class(async,'async')
  checkmate::expect_character(file,max.len = 1)

  file.ext = any(magrittr::equals(tools::file_ext(file),c("rds","RDS")))

  if(!file.ext) {
    stop('output format must be rds')
  }

  saveRDS(async,file)

  return(c('file' = file ))

}



#' @title save_async
#' @description Function to read a single async object saved as rds.
#' @param file a connection or the name of the file where the R object is saved to or read from.
#' @export
load_async = function(file){

  checkmate::expect_character(file,max.len = 1)

  async = readRDS(file)

  checkmate::expect_class(async,'async')

  return(async)

}




