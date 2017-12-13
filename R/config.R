#' @export
openeo <- new.env()
openeo$api.version <- NULL
openeo$project.path <- NULL
openeo$data.path <- NULL
openeo$job.path <- NULL
openeo$api.port <- NULL


initEnvironmentDefault = function() {
  if (is.null(openeo$api.version)) {
    openeo$api.version <- "0.0.1"
  }
  if (is.null(openeo$project.path)) {
    openeo$project.path <- "C:/code/openeo.r.backend"
  }
  if (is.null(openeo$data.path)) {
    openeo$data.path <- "C:/code/openeo.r.backend/data"
  }
  if (is.null(openeo$job.path)) {
    openeo$job.path <- "C:/code/openeo-files/jobs"
  }
  if (is.null(openeo$api.port)) {
    openeo$api.port <- 8000
  }
}

