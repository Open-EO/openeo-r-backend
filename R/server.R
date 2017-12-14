#' Load and Start server
#' 
#' This function loads the api components and starts the server
#' @importFrom plumber plumb
#' @export
startup <- function (port=NA, api.path=NA) {
  # set environment variables if not set externally
  initEnvironmentDefault()
  
  # load descriptions, meta data and file links for provided data sets
  loadData()
  
  # register the processes provided by the server provider
  registerProcesses()
  
  # if there have been previous job postings load those jobs into the system
  loadExistingJobs()
  
  if (!is.na(api.path)) {
    
  } else {
    
  }
  setwd(openeo$project.path)
  
  root <- plumb("R/api.R")
  
  root$run(port = openeo$api.port)
}
