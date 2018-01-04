#' Configuration environment
#' 
#' This environment will contain variables that are used throughout the server start and during server uptime
#' 
#' @field api.version The current api version used
#' @field project.path The filesystem path where to find the plumber api file
#' @field data.path The filesystem path where to find the datasets
#' @field jobs.path The filesystem path where the jobs are stored
#' @field api.port The port where the plumber webservice is working under
#' @field jobs This will be managed during startup. Here all stored jobs under jobs.path are loaded into memory and stored here
#' @field processes This field is also managed during runtime. Here all template processes are listed
#' @field data A list of products offered by the service which is managed at runtime.
#' 
#' @name openeo
#' @export
openeo <- new.env()
openeo$api.version <- NULL
openeo$project.path <- NULL
openeo$data.path <- NULL
openeo$jobs.path <- NULL
openeo$api.port <- NULL


initEnvironmentDefault = function() {

  if (is.null(openeo$project.path)) {
    openeo$project.path <- "C:/code/openeo.r.backend"
  }
  if (is.null(openeo$data.path)) {
    openeo$data.path <- "C:/code/openeo.r.backend/data"
  }
  if (is.null(openeo$job.path)) {
    openeo$jobs.path <- "C:/code/openeo-files/jobs"
  }
  if (is.null(openeo$api.port)) {
    openeo$api.port <- 8000
  }
}

