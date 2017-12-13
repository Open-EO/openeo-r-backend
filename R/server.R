#' Load and Start server
#' 
#' This function loads the api components and starts the server
#' @importFrom plumber plumb
#' @export
startup <- function () {
  initEnvironmentDefault()
  loadData()
  registerProcesses()
  
  setwd(openeo$project.path)
  
  root <- plumb("R/api.R")
  
  root$run(port = openeo$api.port)
}
