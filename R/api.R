# 
# need to use source inclusion at the api level, since plumber
# loads this script and creates the api. any functions / variables
# or else defined at package level is not visible here elsewise.
# 
#' @include Granule-class.R
#' @include Collection-class.R
#' @include Product-class.R
#' @include Job-class.R
#' @include processes.R
#' @include data.R
#' @import raster
library(jsonlite)
openeo$api.version <- "0.0.1"

############################
#
# serverinformation endpoint
#
############################

#* @get /api/version
#* @serializer unboxedJSON
function() {
  list(version=openeo$api.version)
}

#* @get /api/capabilities
#* @serializer unboxedJSON
function() {
  list(
    "/api/data",
    "/api/data/{product_id}",
    "/api/processes",
    "/api/processes/{process_id}",
    "/api/jobs"
  )
}

############################
#
# data endpoint
#
############################

# creates an overview on products available
#* @get /api/data
#* @serializer unboxedJSON
function() {
  datalist = openeo$data
  unname(lapply(datalist, function(l){
    return(l$shortInfo())
  }))
}

# returns details of a certain product
#* @get /api/data/<pid>
#* @serializer unboxedJSON
function(req,res,pid) {
  if (pid %in% names(openeo$data) == FALSE) {
    return(error(res,404,"Product not found"))
  } else {
    return(openeo$data[[pid]]$detailedInfo())
  }
}

############################
#
# processes endpoint
#
############################

# creates an overview on available processes
#* @get /api/processes
#* @serializer unboxedJSON
function() {
  processeslist = openeo$processes
  unname(lapply(processeslist, function(l){
    return(l$shortInfo())
  }))
}

# returns details of a certain product
#* @get /api/processes/<pid>
#* @serializer unboxedJSON
function(req,res,pid) {
  if (pid %in% names(openeo$processes) == FALSE) {
    return(error(res,404,"Product not found"))
  } else {
    return(openeo$processes[[pid]]$detailedInfo())
  }
}

############################
#
# jobs endpoint
#
############################

#* @post /api/jobs
#* @serializer unboxedJSON
function(req,res,evaluate) {
  if (is.null(evaluate) || !evaluate %in% c("lazy","batch")) {
    return(error(res,400, "Missing query parameter \"evaluate\" or it contains a value other then \"lazy\" or \"batch\""))
  }
  
  job = Job$new()
  job$register()
  
  data=list()
  
  process_graph = fromJSON(req$postBody)
  data[["process_graph"]] = process_graph
  
  data[["status"]] = "submitted"
  
  submit_time = Sys.time()
  data[["submitted"]] = submit_time
  job$submitted = submit_time
  
  
  
  job$store(json=toJSON(data,pretty=TRUE,auto_unbox = TRUE))
  
  if (evaluate == "batch") {
    #TODO load processgraph and execute
  }
  
  return(list(
    job_id=job$job_id
  ))
}

#* @delete /api/jobs/<job_id>
#* @serializer unboxedJSON
function(req,res,job_id) {
  if (job_id %in% names(openeo$jobs)) {
    job = openeo$jobs[[job_id]]
    tryCatch(
      {
        job$delete()
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

############################
#
# download endpoint
#
############################

# those are not openeo specification, it is merely a test to execute the job and return data

#* @serializer contentType list(type="image/GTiff")
#* @get /api/download/<job_id>
function(req,res,job_id,format) {
  listedJobs = names(openeo$jobs)
  if (!job_id %in% listedJobs) {
    error(res, 404, paste("Cannot find job with id:",job_id))
  } else {
    job = openeo$jobs[[job_id]]
    result = job$run()
    
    rasterdata = result$granules[[1]]$data
    tmp = tempfile(fileext=".tif")
    writeRaster(x=rasterdata,filename=tmp,format="GTiff")
    
    
    sendFile(res, 
             status=200, 
             job_id, 
             file.ext=".tif", 
             contentType=format,
             data=readBin(tmp, "raw", n=file.info(tmp)$size))
  }
  
  
}

############################
#
# utility functions
#
############################

ok = function(res) {
  error(res,200,"OK")
}

error = function(res, status,msg) {
  res$status = status
  
  return(list(
    status=status,
    message=msg)
    )
}

sendFile = function(res, status, job_id,file.ext, contentType, data) {
  res$status = status
  res$body = data
  res$setHeader("Content-Type", contentType)
  res$setHeader("Content-Disposition", paste("attachment;filename=",job_id,file.ext,sep=""))
  return(res)
}
