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
#' @include Server-class.R
#' @import raster
#' @import plumber
#' @importFrom sodium data_encrypt
#' @importFrom sodium bin2hex
#' @importFrom sodium data_decrypt
#' @importFrom sodium hex2bin
#' @importFrom jsonlite base64_dec
#' @importFrom jsonlite base64_enc

openeo.server$api.version <- "0.0.1"

#
#
# serverinformation endpoint ----
#
#

.version = function() {
  list(version=openeo.server$api.version)
}

.capabilities = function() {
  list(
    "/api/data/",
    "/api/data/{product_id}",
    "/api/processes/",
    "/api/processes/{process_id}",
    "/api/jobs/",
    "/api/download/",
    "/api/users/{user_id}/files",
    "/api/users/{user_id}/files/{rel_path}",
    "/api/users/{user_id}/jobs",
    "/api/auth/login"
  )
}

#
#
# data endpoint ----
#
#

# creates an overview on products available
#* @get /api/data
#* @serializer unboxedJSON
.listData = function() {
  datalist = openeo.server$data
  unname(lapply(datalist, function(l){
    return(l$shortInfo())
  }))
}

# returns details of a certain product
#* @get /api/data/<pid>
#* @serializer unboxedJSON
.describeData = function(req,res,pid) {
  if (pid %in% names(openeo.server$data) == FALSE) {
    return(error(res,404,"Product not found"))
  } else {
    return(openeo.server$data[[pid]]$detailedInfo())
  }
}

#
#
# processes endpoint ----
#
#

# creates an overview on available processes
#* @get /api/processes
#* @serializer unboxedJSON
.listProcesses = function() {
  processeslist = openeo.server$processes
  unname(lapply(processeslist, function(l){
    return(l$shortInfo())
  }))
}

# returns details of a certain product
#* @get /api/processes/<pid>
#* @serializer unboxedJSON
.describeProcess = function(req,res,pid) {
  if (pid %in% names(openeo.server$processes) == FALSE) {
    return(error(res,404,"Process not found"))
  } else {
    return(openeo.server$processes[[pid]]$detailedInfo())
  }
}

#
#
# jobs endpoint ----
#
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
    error(res,404,paste("Job with id:",job_id,"cannot been found"))
  }

  return(res)
}

#* @post /api/jobs
#* @serializer unboxedJSON
.createNewJob = function(req,res,evaluate, format) {
  if (is.null(evaluate) || !evaluate %in% c("lazy","batch","sync")) {
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
  } else if (evaluate == "sync") {
    return(.downloadSimple(req,res,job$job_id,format))
  } else {
    #lazy
    return(list(
      job_id=job$job_id
    ))
  }
  
  
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

#
#
# user data and functions ----
#
#

#* @get /api/users/<userid>/files
#* @serializer unboxedJSON
.listUserFiles = function(req,res,userid) {
    if (paste(userid) == paste(req$user$user_id)) {
      req$user$fileList()
    } else {
      error(res,401,"Not authorized to access other workspaces")
    }
}

#* @get /api/users/<userid>/files/<path>
#* @serializer unboxedJSON
.downloadUserFile = function(req,res,userid,path) {
  if (paste(userid) == paste(req$user$user_id)) {
    path = URLdecode(path)
    
    files = req$user$files
    selection = files[files[,"link"]==path,]
    if (nrow(selection) == 0) {
      error(res, 404,paste("File not found under path:",path))
    } else {
      path.ext = unlist(strsplit(selection[,"link"], "\\.(?=[^\\.]+$)", perl=TRUE))
      
      sendFile(
        res,
        200,
        file.name = path.ext[1],
        file.ext = paste(".",path.ext[2],sep=""),
        data = readBin(rownames(selection),"raw", n=selection$size)
      )
    }
  } else {
    error(res,401,"Not authorized to access others files")
  }
  
}

### this creates corrupted files, something along the line read file / write postbody is off

# @put /api/users/<userid>/files/<path>
# @serializer unboxedJSON
.uploadFile = function(req,res,userid,path) {
  if (paste(userid) == paste(req$user$user_id)) {

    path = URLdecode(path)

    storedFilePath = paste(req$user$workspace,"files",path,sep="/")

    if (file.exists(storedFilePath)) {
      file.remove(storedFilePath)
    }

    dir.split = unlist(strsplit(storedFilePath, "/(?=[^/]+$)", perl=TRUE))
    
    req$rook.input$initialize(req$rook.input$.conn,req$rook.input$.length)
    
    dir.create(dir.split[1],recursive = TRUE,showWarnings = FALSE)
    file.create(storedFilePath,showWarnings = FALSE)
    
    outputFile = file(storedFilePath,"wb")
    writeBin(req$rook.input$read(req$rook.input$.length), con=outputFile, useBytes = TRUE)
    close(outputFile,type="wb")
    ok(res)
  } else {
    error(res,401,"Not authorized to upload data into other users workspaces")
  }
}

#* @delete /api/users/<userid>/files/<path>
#* @serializer unboxedJSON
.deleteUserData = function(req,res,userid,path) {
  if (paste(userid) == paste(req$user$user_id)) {
    user = req$user
    path = URLdecode(path)
    
    storedFilePath = paste(user$workspace,"files",path,sep="/")
    
    files = req$user$files
    selection = files[files[,"link"]==path,]
    if (nrow(selection) == 0) {
      error(res, 404,paste("User has no file under path:",path))
    } else {
      file = rownames(selection)
      
      unlink(file, recursive = TRUE,force=TRUE)
      ok(res)
    }
  } else {
    error(res,401,"Not authorized to delete data of others")
  }
  
}

#* @get /api/users/<userid>/jobs
#* @serializer unboxedJSON
.listUserJobs = function(req,res,userid) {
    if (paste(userid) == paste(req$user$user_id)) {
      user = req$user
      
      
      possibleUserJobs = user$jobs
      jobRepresentation = lapply(possibleUserJobs, function(job_id){
        job = openeo.server$loadJob(job_id)
        return(job$detailedInfo())
      })
      
      return(unname(jobRepresentation))
    } else {
      error(res,401,"Not authorized to view jobs of others")
    }

}

#* @get /api/auth/login
#* @serializer unboxedJSON
.login = function(req,res) {
  auth = req$HTTP_AUTHORIZATION
  encoded=substr(auth,7,nchar(auth))
  
  decoded = rawToChar(base64_dec(encoded))
  user_name = unlist(strsplit(decoded,":"))[1]
  user_pwd = unlist(strsplit(decoded,":"))[2]
  tryCatch(
    {  
      result = dbGetQuery(openeo.server$database, "select * from user where user_name = :name limit 1",param=list(name=user_name))
      if (nrow(result) == 0) {
        stop("Invalid user")
      }
      
      user = as.list(result)
      
      if (user$password == user_pwd) {
        encryption = data_encrypt(charToRaw(paste(user$user_id)),openeo.server$secret.key)
        
        token = bin2hex(append(encryption, attr(encryption,"nonce")))
        
        list(user_id = user$user_id, token=token)
      } else {
        stop("Wrong password")
      }
    },
    error=function(e) {
      error(res,403,"Login failed.")
    }
  )
}

#
#
# download endpoint ----
#
#

# those are not openeo specification, it is merely a test to execute the job and return data

#* @serializer contentType list(type="image/GTiff")
#* @get /api/download/<job_id>
.downloadSimple = function(req,res,job_id,format) {
  if (!openeo.server$jobExists(job_id)) {
    error(res, 404, paste("Cannot find job with id:",job_id))
  } else {
    drivers = gdalDrivers()
    allowedGDALFormats = drivers[drivers$create,"name"]
    if (is.null(format) || !format %in% allowedGDALFormats) {
      return(error(res,400,paste("Format '",format,"' is not supported or recognized by GDAL",sep="")))
    }
    
    job = openeo.server$loadJob(job_id)
    result = job$run()
    
    updated_at = as.character(Sys.time())
    job_status = "finished"
    #TODO consumed credits
    
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


#
#
# pipeline filter ----
#
#

#* @filter checkAuth
.authorized = function(req, res){
  if (req$REQUEST_METHOD == 'OPTIONS') {
    return(forward())
  }
  tryCatch({
    auth = unlist(strsplit(req$HTTP_AUTHORIZATION," "))
    if (auth[1] == "Bearer") {
      token = auth[2]
      hextoken = hex2bin(token)
      nonce.length = 24
      msg = hextoken[1:(length(hextoken)-nonce.length)]
      nonce = hextoken[((length(hextoken)-nonce.length)+1):length(hextoken)]
      
      user_id = rawToChar(data_decrypt(msg,openeo.server$secret.key,nonce))
      
  
      user = openeo.server$loadUser(user_id)
      req$user = user
      
      forward()

    } else {
      stop("Incorrect authentication method.")
    }
  },
  error = function(e) {
    error(res,401,"Unauthorized")
  }
  )
}

.cors_filter = function(req,res) {
  res$setHeader("Access-Control-Allow-Origin", req$HTTP_ORIGIN)
  res$setHeader("Access-Control-Allow-Credentials", "true")
  plumber::forward()
}

.cors_option_bypass = function(req,res) {
  res$setHeader("Access-Control-Allow-Headers", "Authorization, Accept, Content-Type")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH")
  ok(res);
}

#
#
# utility functions ----
#
#

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

sendFile = function(res, status, file.name = NA,file.ext=NA, contentType=NA, data) {
  res$status = status
  res$body = data
  if (! is.na(contentType)) {
    res$setHeader("Content-Type", contentType)
  }
  if (! is.na(file.name) && !is.na(file.ext)) {
    res$setHeader("Content-Disposition", paste("attachment;filename=",file.name,file.ext,sep=""))
  } else if (!is.na(file.name) && is.na(file.ext)) {
    res$setHeader("Content-Disposition", paste("attachment;filename=",file.name,sep=""))
  }
  return(res)
}

#
#
# setup the routes ----
#
#

createAPI = function() {
  root = plumber$new()
  
  root$handle("GET",
              "/api/version",
              handler = .version,
              serializer = serializer_unboxed_json())
  root$handle("OPTIONS",
              "/api/version",
              handler = .cors_option_bypass)
  
  root$handle("GET",
              "/api/capabilities",
              handler = .capabilities,
              serializer = serializer_unboxed_json())
  
  root$handle("OPTIONS",
              "/api/capabilities",
              handler = .cors_option_bypass)
  
  root$registerHook("postroute",.cors_filter)
  
  data = plumber$new()
  
  data$handle("GET",
              "/",
              handler = .listData,
              serializer = serializer_unboxed_json())
  data$handle("OPTIONS",
              "/",
              handler = .cors_option_bypass)
  
  data$handle("GET",
              "/<pid>",
              handler = .describeData,
              serializer = serializer_unboxed_json())
  data$handle("OPTIONS",
              "/<pid>",
              handler = .cors_option_bypass)
  
  root$mount("/api/data",data)
  
  process = plumber$new()
  
  process$handle("GET",
                 "/",
                 handler = .listProcesses,
                 serializer = serializer_unboxed_json())
  process$handle("OPTIONS",
              "/",
              handler = .cors_option_bypass)
  
  process$handle("GET",
                 "/<pid>",
                 handler = .describeProcess,
                 serializer = serializer_unboxed_json())
  process$handle("OPTIONS",
                 "/<pid>",
                 handler = .cors_option_bypass)
  
  root$mount("/api/processes",process)
  
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
  
  jobs$handle("DELETE",
              "/<job_id>",
              handler = .deleteJob,
              serializer = serializer_unboxed_json())

  
  jobs$filter("authorization",.authorized)
  
  root$mount("/api/jobs",jobs)
  
  users = plumber$new()
  
  users$handle("GET",
               "/<userid>/files",
               handler = .listUserFiles,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
              "/<userid>/files",
              handler = .cors_option_bypass)
  
  users$handle("GET",
               "/<userid>/files/<path>",
               handler = .downloadUserFile,
               serializer = serializer_unboxed_json())
  
  users$handle("PUT",
               "/<userid>/files/<path>",
               handler = .uploadFile,
               serializer = serializer_unboxed_json())
  
  users$handle("DELETE",
               "/<userid>/files/<path>",
               handler = .deleteUserData,
               serializer = serializer_unboxed_json())
  
  users$handle("OPTIONS",
               "/<userid>/files/<path>",
               handler = .cors_option_bypass)
  
  users$handle("GET",
               "/<userid>/jobs",
               handler = .listUserJobs,
               serializer = serializer_unboxed_json())
  
  users$handle("OPTIONS",
               "/<userid>/jobs",
               handler = .cors_option_bypass)
  
  users$filter("authorization",.authorized)
  
  root$mount("/api/users",users)
  
  authentication = plumber$new()
  
  authentication$handle(c("GET","POST"),
                        "/login",
                        handler = .login,
                        serializer = serializer_unboxed_json())
  
  authentication$handle("OPTIONS",
                       "/login",
                       handler = .cors_option_bypass)
  
  root$mount("/api/auth",authentication)
  
  download = plumber$new()
  
  download$handle("GET",
                  "/<job_id>",
                  handler = .downloadSimple,
                  serializer = serializer_unboxed_json())
  
  download$handle("OPTIONS",
                  "/<job_id>",
                  handler = .cors_option_bypass)
  
  download$filter("authorization", .authorized)
  
  root$mount("/api/download",download)
  
  return(root)
}