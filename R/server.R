#' Load and Start server
#' 
#' This function loads the api components and starts the server
#' @export
startup <- function () {
  root <- plumber::plumb("R/api.R")
  
  root$run(port=8000)
}
