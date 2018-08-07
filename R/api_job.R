# jobs endpoint ====

createJobsEndpoint = function() {
  jobs = plumber$new()
  
  jobs$handle("GET",
              "/<job_id>",
              handler = .describeJob,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>",
              handler = .cors_option_bypass)
  
  jobs$handle("POST",
              "/",
              handler = .createNewJob,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/",
              handler = .cors_option_bypass)
  
  
  jobs$handle("GET",
              "/<job_id>/subscribe",
              handler = .not_implemented_yet,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>/subscribe",
              handler = .cors_option_bypass)
  
  
  jobs$handle("PATCH",
              "/<job_id>/queue",
              handler = .performJob,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>/queue",
              handler = .cors_option_bypass)
  
  
  jobs$handle("GET",
              "/<job_id>/pause",
              handler = .not_implemented_yet,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>/pause",
              handler = .cors_option_bypass)
  
  
  jobs$handle("GET",
              "/<job_id>/cancel",
              handler = .not_implemented_yet,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>/cancel",
              handler = .cors_option_bypass)
  
  
  jobs$handle("GET",
              "/<job_id>/download",
              handler = .createDownloadableFileList,
              serializer = serializer_json())
  jobs$handle("OPTIONS",
              "/<job_id>/download",
              handler = .cors_option_bypass)
  
  jobs$handle("DELETE",
              "/<job_id>",
              handler = .deleteJob,
              serializer = serializer_unboxed_json())

  
  
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
  # TODO check if postBody is valid
  process_graph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
  # TODO check if this is the simple representation or the complex (probably correct version)
  # this means search for "args" lists if (names(...$args) == NULL) => unlist(...$args, recursive = FALSE)
  process_graph = .createSimpleArgList(process_graph)
  
  job = Job$new(user_id = req$user$user_id, process_graph = process_graph)
  submit_time = Sys.time()
  job$status = "submitted"
  job$submitted = submit_time
  job$last_update = submit_time
  
  job$store()
  
  result = list(
    job_id=job$job_id,
    status = job$status,
    updated = job$last_update,
    submitted = job$submitted,
    user_id = job$user_id,
    consumed_credits = job$consumed_credits
  )
  
  return(result)
}

# TODO remove, because it is probably obsolete
.createSimpleArgList = function(graph) {
  if ("args" %in% names(graph)) {
    
    if (is.null(names(graph$args))) {
      args = unlist(graph$args,recursive = FALSE)
      
      #named list it should be
      for (index in names(args)) {
        elem = args[[index]]
        if ("args" %in% names(elem)) {
          args[[index]] = .createSimpleArgList(elem)
        }
      }
      graph$args = args
    }
  }
  return(graph)
}

.createDownloadableFileList = function(req,res,job_id) {
  if (!exists.Job(job_id)) {
    error(res, 404, paste("Cannot find job with id:",job_id))
  } else {
    job_results = paste(openeo.server$workspaces.path,"jobs",job_id,sep="/")
    
    base_url = paste(openeo.server$baseserver.url,"result/",job_id,sep="")
    
    #get files in outputfolder but not the log file
    paste(base_url,list.files(job_results,pattern="[^process\\.log]"),sep="/")
  }
}

.deleteJob = function(req,res,job_id) {
  job = Job$new(job_id)
  success = job$remove()
  
  if (success) {
    ok(res)
  } else {
    error(res, 404 ,"Cannot delete job. Either it is already deleted or the job_id is not valid.")
  }
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

  ok(res)
}