############################
#
# jobs endpoint
#


createJobsEndpoint = function() {
  jobs = plumber$new()
  
  jobs$handle("GET",
              "/<jobid>",
              handler = .describeJob,
              serializer = serializer_unboxed_json())
  jobs$handle("DELETE",
              "/<job_id>",
              handler = .deleteJob,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<jobid>",
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
  
  
  jobs$handle("GET",
              "/<job_id>/queue",
              handler = .not_implemented_yet,
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
              handler = .not_implemented_yet,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>/download",
              handler = .cors_option_bypass)
  
  
  jobs$filter("authorization",.authorized)
  
  return(jobs)
}

############################
#
# Request handling functions
#

#* @get /api/jobs/<jobid>
.describeJob = function(req,res,jobid) {
  if (openeo.server$jobExists(jobid)) {
    job = openeo.server$loadJob(jobid)
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
    error(res,404,paste("Job with id:",jobid,"cannot been found"))
  }
  
  return(res)
}

#* @post /api/jobs
#* @serializer unboxedJSON
.createNewJob = function(req,res,evaluate) {
  if (is.null(evaluate) || !evaluate %in% c("lazy","batch")) {
    return(error(res,400, "Missing query parameter \"evaluate\" or it contains a value other then \"lazy\" or \"batch\""))
  }
  # TODO check if postBody is valid
  process_graph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
  # TODO check if this is the simple representation or the complex (probably correct version)
  # this means search for "args" lists if (names(...$args) == NULL) => unlist(...$args, recursive = FALSE)
  process_graph = .createSimpleArgList(process_graph)
  
  job = openeo.server$createJob(user = req$user, process_graph = process_graph)
  submit_time = Sys.time()
  job$status = "submitted"
  job$evaluation = evaluate
  job$submitted = submit_time
  job$last_update = submit_time
  
  job$store(con=openeo.server$database)
  
  if (evaluate == "batch") {
    #TODO load processgraph and execute
  }
  
  return(list(
    job_id=job$job_id
  ))
}

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

#* @delete /api/jobs/<job_id>
#* @serializer unboxedJSON
.deleteJob = function(req,res,job_id) {
  if (openeo.server$jobExists(job_id)) {
    job = openeo.server$loadJob(job_id)
    tryCatch(
      {
        openeo.server$delete(job)
        ok(res)
      }, 
      error = function(err) {
        error(res,"500",err$message)
      }
    )
  } else {
    error(res,404,paste("Job with id:",job_id,"cannot been found"))
  }
}