#' @export
openeo <- new.env()


initEnvironment = function() {
  openeo$api.version <- "0.0.1"
  openeo$project.path <- "C:/code/openeo.r.backend"
  openeo$data.path <- "C:/code/openeo.r.backend/data"
  openeo$job.path <- "C:/code/openeo-files/jobs"
}

