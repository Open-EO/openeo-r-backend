#' @include utils.R
NULL

# jobs endpoint ----

createJobsEndpoint = function() {
  jobs = plumber$new()
  
  # get user jobs ====
  openeo.server$registerEndpoint("/jobs/","GET")
  jobs$handle("GET",
            "/",
            handler = .listUserJobs,
            serializer = serializer_unboxed_json())
  
  # create new job ====
  openeo.server$registerEndpoint("/jobs/","POST")
  jobs$handle("POST",
              "/",
              handler = .createNewJob,
              serializer = serializer_unboxed_json())
  
  jobs$handle("OPTIONS",
              "/",
              handler = .cors_option_bypass)
  
  # describe job in detail ====
  openeo.server$registerEndpoint("/jobs/{job_id}","GET")
  jobs$handle("GET",
              "/<job_id>",
              handler = .describeJob,
              serializer = serializer_unboxed_json())
  
  # modify job ====
  openeo.server$registerEndpoint("/jobs/{job_id}","PATCH")
  jobs$handle("PATCH",
              "/<job_id>",
              handler = .updateJob,
              serializer = serializer_unboxed_json())

  # delete job ====
  openeo.server$registerEndpoint("/jobs/{job_id}","DELETE")
  jobs$handle("DELETE",
              "/<job_id>",
              handler = .deleteJob,
              serializer = serializer_unboxed_json())
  
  jobs$handle("OPTIONS",
              "/<job_id>",
              handler = .cors_option_bypass)
  
  
  # perform job ====
  openeo.server$registerEndpoint("/jobs/{job_id}/results","POST")
  jobs$handle("POST",
              "/<job_id>/results",
              handler = .performJob,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>/results",
              handler = .cors_option_bypass)
  
  # get info about finished job results ====
  openeo.server$registerEndpoint("/jobs/{job_id}/results","GET")
  jobs$handle("GET",
              "/<job_id>/results",
              handler = .createDownloadableFileList,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>/results",
              handler = .cors_option_bypass)
  
  # 
  # jobs$handle("GET",
  #             "/<job_id>/subscribe",
  #             handler = .not_implemented_yet,
  #             serializer = serializer_unboxed_json())
  # jobs$handle("OPTIONS",
  #             "/<job_id>/subscribe",
  #             handler = .cors_option_bypass)
  # 
  # jobs$handle("GET",
  #             "/<job_id>/pause",
  #             handler = .not_implemented_yet,
  #             serializer = serializer_unboxed_json())
  # jobs$handle("OPTIONS",
  #             "/<job_id>/pause",
  #             handler = .cors_option_bypass)
  # 
  # 
  # jobs$handle("GET",
  #             "/<job_id>/cancel",
  #             handler = .not_implemented_yet,
  #             serializer = serializer_unboxed_json())
  # jobs$handle("OPTIONS",
  #             "/<job_id>/cancel",
  #             handler = .cors_option_bypass)
  
  # job cost estimation ====
  openeo.server$registerEndpoint("/jobs/{job_id}/estimate","GET")
  jobs$handle("GET",
              "/<job_id>/estimate",
              handler = .estimateJobCosts,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>/estimate",
              handler = .cors_option_bypass)

  
  jobs$filter("authorization",.authorized)
  jobs$filter("me_filter", .replace_user_me_in_body)
  
  return(jobs)
}

# handler functions ====

#* @get /api/jobs/<jobid>
.describeJob = function(req,res,job_id) {
  if (exists.Job(job_id)) {
    job = Job$new(job_id=job_id)
    job$load()
    
    tryCatch(
      {
        res$body = toJSON(job$detailedInfo(),na="null",null="null",auto_unbox = TRUE)
        res$setHeader("Content-Type","application/json")
      }, 
      error = function(err) {
        error(res,"500",err$message)
      }
    )
  } else {
    error(res,404,paste("Job with id:",job_id,"cannot been found"))
  }
  
  return(res)
}

#* @post /api/jobs
#* @serializer unboxedJSON
.createNewJob = function(req,res) {
  
  sent_job = fromJSON(req$postBody,simplifyDataFrame = FALSE)
  
  # TODO check if postBody is valid
  process_graph = sent_job$process_graph
  
  job = Job$new(user_id = req$user$user_id, process_graph = process_graph)
  submit_time = Sys.time()
  job$status = "submitted"
  job$submitted = submit_time
  job$last_update = submit_time
  
  if (!is.null(sent_job$title)) job$title = sent_job$title
  if (!is.null(sent_job$description)) job$description = sent_job$description
  if (!is.null(sent_job$output)) job$output = sent_job$output
  if (!is.null(sent_job$plan)) job$plan = sent_job$plan
  if (!is.null(sent_job$budget)) job$budget = sent_job$budget
  
  job$store()
  res$setHeader(name = "Location",
                value= paste(openeo.server$baseserver.url,"jobs/",job$job_id,sep=""))
  res$status = 201
  
}

.createDownloadableFileList = function(req,res,job_id) {
  if (!exists.Job(job_id)) {
    error(res, 404, paste("Cannot find job with id:",job_id))
  } else {
    job = Job$new(job_id = job_id)
    job$load()
    
    if (job$status != "finished") {
      error(res,400,paste("Job '",job_id,"' has not finished or has not been started yet. Please try again later.",sep=""))
    }
    
    job_results = paste(openeo.server$workspaces.path,"jobs",job_id,sep="/")
    
    base_url = paste(openeo.server$baseserver.url,"result/",job_id,sep="")
    
    #get files in outputfolder but not the log file
    links = paste(base_url,list.files(job_results,pattern="[^process\\.log]"),sep="/")
    
    result_links = list()
    for (i in 1:length(links)) {
      result_links = c(result_links, list(list(href = links[i])))
    }
    
    return(list(
      title = job$title,
      description = job$description,
      updated = iso_datetime(job$last_update),
      links = result_links
    ))
  }
}

.deleteJob = function(req,res,job_id) {
  job = Job$new(job_id)
  success = job$remove()
  
  if (success) {
    res$status = 204
  } else {
    error(res, 404 ,"Cannot delete job. Either it is already deleted or the job_id is not valid.")
  }
}

.updateJob = function(req,res,job_id) {
  sent_job = fromJSON(req$postBody,simplifyDataFrame = FALSE)
  
  job = Job$new(user_id = req$user$user_id, job_id = job_id)
  job$load()
  
  if (!is.null(sent_job$process_graph) && job$status %in% c("submitted","canceled","error")) {
    process_graph = sent_job$process_graph
    
    job$modifyProcessGraph(process_graph)
  }
  
  if (!is.null(sent_job$title)) {
    job$title = sent_job$title
  }
  
  if (!is.null(sent_job$description)) {
    job$description = sent_job$description
  }
  
  if (!is.null(sent_job$output)) {
    job$output = sent_job$output
  }
  
  if (!is.null(sent_job$plan)) {
    job$plan = sent_job$plan
  }
  
  if (!is.null(sent_job$budget)) {
    job$budget = sent_job$budget
  }
  
  
  job$last_update = Sys.time()
  
  job$store()

  res$status = 204
}

.performJob = function(req,res,job_id) {
  
  if (is.null(job_id) || !exists.Job(job_id)) {
    stop("Job does not exist")
  }
  
  job = Job$new(job_id=job_id)
  job$load()
  
  plan(multiprocess)
  
  processing <- future({
    openeo.server$runJob(job= job)
  }, packages=c("openEO.R.Backend","raster","RSQLite","DBI","rgdal","gdalUtils"))

  res$status = 202
}

#* @get /api/jobs/
#* @serializer unboxedJSON
.listUserJobs = function(req,res) {
  user = req$user
  
  jobRepresentation = lapply(user$jobs, function(job_id){
    job = Job$new(job_id)
    job$load()
    return(job$shortInfo())
  })
  
  return(list(
    jobs=unname(jobRepresentation),
    links=list()
  ))

}

# for now this will only contain a single "hard coded" entry
.estimateJobCosts = function(req,res, job_id) {
  return(list(
    costs = 0,
    duration = "PT2M",
    downloads_included = TRUE
  ))
}