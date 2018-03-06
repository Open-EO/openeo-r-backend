# jobs endpoint ====

createJobsEndpoint = function() {
  jobs = plumber$new()
  
  jobs$handle("GET",
              "/<jobid>",
              handler = .describeJob,
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
              handler = .downloadSimple,
              serializer = serializer_unboxed_json())
  jobs$handle("OPTIONS",
              "/<job_id>/download",
              handler = .cors_option_bypass)
  
  
  jobs$filter("authorization",.authorized)
  
  return(jobs)
}

# handler functions ====

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
.createNewJob = function(req,res) {

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
  
  result = list(
    job_id=job$job_id,
    status = job$status,
    updated = job$last_update,
    submitted = job$submitted,
    user_id = job$user_id,
    consumed_credits = job$consumed_credits
  )
  
  return()
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

# those are not openeo specification, it is merely a test to execute the job and return data

#* @serializer contentType list(type="image/GTiff")
#* @get /api/download/<job_id>
.downloadSimple = function(req,res,job_id,format) {
  if (!openeo.server$jobExists(job_id)) {
    error(res, 404, paste("Cannot find job with id:",job_id))
  } else {
    job = openeo.server$loadJob(job_id)
    result = job$run()
    
    updated_at = as.character(Sys.time())
    job_status = "finished"
    
    con = openeo.server$getConnection()
    updateJobQuery = "update job set last_update = :time, status = :status where job_id = :job_id"
    dbExecute(con, updateJobQuery ,param=list(time=updated_at,
                                              status=job_status,
                                              job_id=job_id))
    
    
    rasterdata = result$granules[[1]]$data #TODO handle multi granules...
    
    rasterfile = writeRaster(x=rasterdata,filename=tempfile(),format=format)
    
    
    tryCatch({
      sendFile(res, 
               status=200, 
               file.name="output", 
               contentType=paste("application/gdal-",format,sep=""),
               data=readBin(rasterfile@file@name, "raw", n=file.info(rasterfile@file@name)$size))
    },finally = function(rasterfile) {
      unlink(rasterfile@file@name)
    })
  }
}