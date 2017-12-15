#' @include Job-class.R

loadExistingJobs = function() {
  openeo$jobs = list()
  
  for (jobid in list.files(openeo$jobs.path)) {
    job = Job$new(job_id=jobid)
    job$register()
  }
}