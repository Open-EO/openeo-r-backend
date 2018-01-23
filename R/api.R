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

############################
#
# serverinformation endpoint
#
############################

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

############################
#
# data endpoint
#
############################

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

############################
#
# processes endpoint
#
############################

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
    return(error(res,404,"Product not found"))
  } else {
    return(openeo.server$processes[[pid]]$detailedInfo())
  }
}

############################
#
# jobs endpoint
#
############################

#* @get /api/jobs/<jobid>
.describeJob = function(req,res,jobid) {
  if (!jobid %in% names(openeo.server$jobs)) {
    error(res,404,paste("Job with job_id",jobid," was not found"))
  } else {
    res$body = toJSON(openeo.server$jobs[[jobid]]$detailedInfo(),na="null",null="null",auto_unbox = TRUE)
    res$setHeader("Content-Type","application/json")
  }
  return(res)
}

#* @post /api/jobs
#* @serializer unboxedJSON
.createNewJob = function(req,res,evaluate) {
  if (is.null(evaluate) || !evaluate %in% c("lazy","batch")) {
    return(error(res,400, "Missing query parameter \"evaluate\" or it contains a value other then \"lazy\" or \"batch\""))
  }
  
  job = openeo.server$createJob(user = req$user)
  
  openeo.server$register(job)
  
  data=list()
  
  process_graph = fromJSON(req$postBody)
  data[["process_graph"]] = process_graph
  
  data[["status"]] = "submitted"
  data[["evaluation"]] = evaluate
  
  submit_time = Sys.time()
  data[["submitted"]] = submit_time
  job$submitted = submit_time
  
  
  job$store(json = toJSON(data,pretty=TRUE,auto_unbox = TRUE))
  
  if (evaluate == "batch") {
    #TODO load processgraph and execute
  }
  
  return(list(
    job_id=job$job_id
  ))
}

#* @delete /api/jobs/<job_id>
#* @serializer unboxedJSON
.deleteJob = function(req,res,job_id) {
  if (job_id %in% names(openeo.server$jobs)) {
    job = openeo.server$jobs[[job_id]]
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

############################
#
# user data and functions
#
############################

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
# .uploadFile = function(req,res,userid,path) {
#     if (! userid %in% names(openeo.server$users)) {
#       error(res,404,paste("User id with id \"",userid, "\" was not found", sep=""))
#     } else {
#       user = openeo.server$users[[userid]]
#       path = URLdecode(path)
#   
#       storedFilePath = paste(user$workspace,"files",path,sep="/")
#       
#       if (file.exists(storedFilePath)) {
#         file.remove(storedFilePath)
#       }
#       
#       dir.split = unlist(strsplit(storedFilePath, "/(?=[^/]+$)", perl=TRUE))
#   
#       message(names(req$rook.input))
#       # binaryPost = readBin(con=req$rook.input,what="character")
#       binaryPost = req$postBody
#       dir.create(dir.split[1],recursive = TRUE,showWarnings = FALSE)
#       file.create(storedFilePath,showWarnings = FALSE)
# 
#       #TODO we should check if the input is binary or text
#       writeBin(object=binaryPost,con=file(storedFilePath,"wb"))
# 
#       ok(res)
#     }
# }

#* @delete /api/users/<userid>/files/<path>
#* @serializer unboxedJSON
.deleteUserData = function(req,res,userid,path) {
  if (paste(userid) == paste(req$user$user_id)) {
    user = req$user
    path = URLdecode(path)
    
    storedFilePath = paste(user$workspace,"files",path,sep="/")
    
    files = openeo.server$users[[paste(userid)]]$files
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
      foundIndices = which(possibleUserJobs %in% names(openeo.server$jobs))
      userJobsIds = possibleUserJobs[foundIndices]
      
      userJobs = openeo.server$jobs[userJobsIds]
      jobRepresentation = lapply(userJobs, function(job){
        return(job$detailedInfo())
      })
      names(jobRepresentation) <- NULL
      return(jobRepresentation)
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
  user_pwd = unlist(strsplit(decoded,":"))
  
  tryCatch(
    {  
      user = openeo.server$getUserByName(user_pwd[1])
      if (user$password == user_pwd[2]) {
        encryption = data_encrypt(charToRaw(paste(user$user_id)),openeo.server$secret.key)
        
        token = bin2hex(append(encryption, attr(encryption,"nonce")))
        
        list(token=token)
      } else {
        stop("Wrong password")
      }
    },
    error=function(e) {
      error(res,404,"Login failed.")
    }
  )
}

############################
#
# download endpoint
#
############################

# those are not openeo specification, it is merely a test to execute the job and return data

#* @serializer contentType list(type="image/GTiff")
#* @get /api/download/<job_id>
.downloadSimple = function(req,res,job_id,format) {
  listedJobs = names(openeo.server$jobs)
  if (!job_id %in% listedJobs) {
    error(res, 404, paste("Cannot find job with id:",job_id))
  } else {
    job = openeo.server$jobs[[job_id]]
    result = job$run()
    
    rasterdata = result$granules[[1]]$data
    tmp = tempfile(fileext=".tif")
    writeRaster(x=rasterdata,filename=tmp,format="GTiff")
    
    
    sendFile(res, 
             status=200, 
             file.name=job_id, 
             file.ext=".tif", 
             contentType=format,
             data=readBin(tmp, "raw", n=file.info(tmp)$size))
  }
}

############################
#
# pipeline filter
#
############################

#* @filter checkAuth
.authorized = function(req, res){
  tryCatch({
    auth = unlist(strsplit(req$HTTP_AUTHORIZATION," "))
    if (auth[1] == "Bearer") {
      token = auth[2]
      hextoken = hex2bin(token)
      nonce.length = 24
      msg = hextoken[1:(length(hextoken)-nonce.length)]
      nonce = hextoken[((length(hextoken)-nonce.length)+1):length(hextoken)]
      
      user_id = rawToChar(data_decrypt(msg,openeo.server$secret.key,nonce))
      if (user_id %in% names(openeo.server$users)) {
        req$user = openeo.server$users[[user_id]]
        forward()
      } else {
        stop("Incorrect token")
      }
    } else {
      stop("Incorrect authentication method.")
    }
  },
  error = function(e) {
    error(res,401,"Unauthorized")
  }
  )
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

sendFile = function(res, status, file.name = NA,file.ext=NA, contentType=NA, data) {
  res$status = status
  res$body = data
  if (! is.na(contentType)) {
    res$setHeader("Content-Type", contentType)
  }
  if (! is.na(file.name) && !is.na(file.ext)) {
    res$setHeader("Content-Disposition", paste("attachment;filename=",file.name,file.ext,sep=""))
  }
  return(res)
}

############################
#
# setup the routes
#
############################

createAPI = function() {
  root = plumber$new()
  
  root$handle("GET",
              "/api/version",
              handler = .version,
              serializer = serializer_unboxed_json())
  
  root$handle("GET",
              "/api/capabilities",
              handler = .capabilities,
              serializer = serializer_unboxed_json())
  
  data = plumber$new()
  
  data$handle("GET",
              "/",
              handler = .listData,
              serializer = serializer_unboxed_json())
  
  data$handle("GET",
              "/<pid>",
              handler = .describeData,
              serializer = serializer_unboxed_json())
  
  root$mount("/api/data",data)
  
  process = plumber$new()
  
  process$handle("GET",
                 "/",
                 handler = .listProcesses,
                 serializer = serializer_unboxed_json())
  
  process$handle("GET",
                 "/<pid>",
                 handler = .describeProcess,
                 serializer = serializer_unboxed_json())
  
  root$mount("/api/processes",process)
  
  jobs = plumber$new()
  
  jobs$handle("GET",
              "/<jobid>",
              handler = .describeJob,
              serializer = serializer_unboxed_json())
  
  jobs$handle("POST",
              "/",
              handler = .createNewJob,
              serializer = serializer_unboxed_json())
  
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
  
  users$handle("GET",
               "/<userid>/files/<path>",
               handler = .downloadUserFile,
               serializer = serializer_unboxed_json())
  
  # users$handle("PUT",
  #              "/<userid>/files/<path>",
  #              handler = .uploadFile,
  #              serializer = serializer_unboxed_json())
  
  users$handle("DELETE",
               "/<userid>/files/<path>",
               handler = .deleteUserData,
               serializer = serializer_unboxed_json())
  
  users$handle("GET",
               "/<userid>/jobs",
               handler = .listUserJobs,
               serializer = serializer_unboxed_json())
  
  users$filter("authorization",.authorized)
  
  root$mount("/api/users",users)
  
  authentication = plumber$new()
  
  authentication$handle(c("GET","POST"),
                        "/login",
                        handler = .login,
                        serializer = serializer_unboxed_json())
  
  root$mount("/api/auth",authentication)
  
  download = plumber$new()
  
  download$handle(
    "GET",
    "/<job_id>",
    handler = .downloadSimple,
    serializer = serializer_unboxed_json())
  
  download$filter("authorization", .authorized)
  
  root$mount("/api/download",download)
  
  return(root)
}