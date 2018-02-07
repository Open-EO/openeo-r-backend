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
#' @include api_data.R
#' @include api_job.R
#' @include api_users.R
#' @include api_services.R
#' @include api_udf_runtimes.R
#' @include api_processes.R
#' @import raster
#' @import plumber
#' @importFrom sodium data_encrypt
#' @importFrom sodium bin2hex
#' @importFrom sodium data_decrypt
#' @importFrom sodium hex2bin
#' @importFrom jsonlite base64_dec
#' @importFrom jsonlite base64_enc

openeo.server$api.version <- "0.0.2"

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

############################
#
# download endpoint
#
############################

# those are not openeo specification, it is merely a test to execute the job and return data

#* @serializer contentType list(type="image/GTiff")
#* @get /api/download/<job_id>
.downloadSimple = function(req,res,job_id,format) {
  if (!openeo.server$jobExists(job_id)) {
    error(res, 404, paste("Cannot find job with id:",job_id))
  } else {
    job = openeo.server$loadJob(job_id)
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

.cors_filter = function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Credentials", TRUE)
  plumber::forward()
}

.cors_option_bypass = function(req,res, ...) {
  res$setHeader("Access-Control-Allow-Headers", "Authorization, Accept, Content-Type")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH")
  
  return(res)
}

.not_implemented_yet = function(req,res, ...) {
  error(res,501, "Not implemented, yet")
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
  
  data = createDataEndpoint()
  root$mount("/api/data",data)
  
  process = createProcessesEndpoint()
  root$mount("/api/processes",process)
  
  jobs = createJobsEndpoint()
  root$mount("/api/jobs",jobs)
  
  users = createUsersEndpoint()
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
  
  executeSynchronous = plumber$new()
  executeSynchronous$handle("POST",
                            "/",
                            handler = .not_implemented_yet,
                            serializer = serializer_unboxed_json())
  executeSynchronous$handle("OPTIONS",
                            "/",
                            handler = .cors_option_bypass)
  executeSynchronous$filter("authorization",.authorized)
  root$mount("/api/execute",executeSynchronous)
  
  services = createServicesEndpoint()
  root$mount("/api/services",services)
  
  udf_runtimes = createUDFRuntimesEndpoint()
  root$mount("/api/udf_runtimes",udf_runtimes)
  
  return(root)
}