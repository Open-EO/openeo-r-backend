#' Load and Start server
#' 
#' This function loads the api components and starts the server
#' @importFrom plumber plumb
#' @export
startup <- function () {
  initEnvironment()
  loadData()
  
  setwd(openeo$project.path)
  
  root <- plumb("R/api.R")
  
  root$run(port=8000)
}
