#' @include utils.R
NULL

# /jobs handler functions ----

#* @get /api/jobs/<jobid>
.describeJob = function(req,res,job_id) {
  tryCatch({
    if (exists.Job(job_id)) {
      job = Job$new(job_id=job_id)
      job$load()
      
      tryCatch(
        {
          res$body = toJSON(job$detailedInfo(),na="null",null="null",auto_unbox = TRUE)
          res$setHeader("Content-Type","application/json")
        }, 
        error = function(err) {
          throwError("Internal",message=err)
        }
      )
    } else {
      throwError("JobNotFound")
    }
    
    return(res)
  }, error=handleError)
}

#* @post /api/jobs
#* @serializer unboxedJSON
.createNewJob = function(req,res) {
  tryCatch({
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
                  value= paste0(openeo.server$configuration$baseserver.url,"/","jobs/",job$job_id))
    res$setHeader(name = "OpenEO-Identifier",value=job$job_id)
    res$status = 201
    
    return(res)
  },error=handleError)
  
}

.createDownloadableFileList = function(req,res,job_id) {
  tryCatch({
    if (!exists.Job(job_id)) {
      throwError("JobNotFound")
    } else {
      job = Job$new(job_id = job_id)
      job$load()
      
      if (job$status == "submitted") {
        throwError("JobNotStarted")
      } else if (job$status != "finished") {
        throwError("JobNotFinished")
      }
      # TODO add notification if job had failed already
      
      job_results = paste(openeo.server$configuration$workspaces.path,"jobs",job_id,sep="/")
      
      base_url = paste0(openeo.server$configuration$baseserver.url,"/","result/",job_id)
      
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
  },error=handleError)
  
}

.deleteJob = function(req,res,job_id) {
  tryCatch({
    job = Job$new(job_id)
    success = job$remove()
    
    if (success) {
      res$status = 204
      return(res)
    } else {
      throwError("JobNotFound")
    }
  },error=handleError)
  
}

.updateJob = function(req,res,job_id) {
  tryCatch({
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
    
    return(res)
  },error=handleError)
  
}

.performJob = function(req,res,job_id) {
  tryCatch({
    if (is.null(job_id) || !exists.Job(job_id)) {
      throwError("JobNotFound")
    }
    
    job = Job$new(job_id=job_id)
    job$load()
    
    plan(multiprocess)
    
    processing <- future({
      openeo.server$runJob(job= job)
    }, packages=c("openEO.R.Backend","raster","RSQLite","DBI","rgdal","gdalUtils"))
    
    res$status = 202
    
    return(res)
  },error=handleError)
  
}

#* @get /api/jobs/
#* @serializer unboxedJSON
.listUserJobs = function(req,res) {
  tryCatch({
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
  }, error = handleError)
  

}

# for now this will only contain a single "hard coded" entry
.estimateJobCosts = function(req,res, job_id) {
  return(list(
    costs = 0,
    duration = "PT2M",
    downloads_included = TRUE
  ))
}