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

#
# serverinformation endpoint ====
#

.version = function() {
  list(version=openeo.server$api.version)
}

.capabilities = function() {
  list(
    "/auth/login",
    "/capabilities",
    "/capabilities/output_formats",
    "/capabilities/services",
    "/data/",
    "/data/{product_id}",
    "/jobs/",
    "/jobs/{job_id}/download",
    "/jobs/{job_id}/queue",
    "/processes/",
    "/processes/{process_id}",
    "/execute/",
    "/services/",
    "/users/{user_id}/files",
    "/users/{user_id}/files/{path}",
    "/users/{user_id}/jobs",
    "/users/{user_id}/process_graphs",
    "/users/{user_id}/process_graphs/{graph_id}",
    
    
  )
}

.output_formats = function() {
  formats = c(openeo.server$outputGDALFormats,openeo.server$outputOGRFormats)
  namedList = lapply(formats,function(format) {
    res = list(gdalformat=format)
    return(res)
  })

  names(namedList) = formats
  
  return(list(
    default="GTiff",
    formats = namedList
  ))
}

.services = function() {
  return(list(
    "wms"
  ))
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
      con = openeo.server$getConnection()
      result = dbGetQuery(con, "select * from user where user_name = :name limit 1",param=list(name=user_name))
      dbDisconnect(con)
      
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
# download endpoint ====
#

.executeSynchronous = function(req,res,format=NULL) {

  if (!is.null(req$postBody)) {
    process_graph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
    output = process_graph$output
    
    if (is.null(format)) {
      format = output$format
    } 
    
    if (is.null(format) || !(format %in% openeo.server$outputGDALFormats || format %in% openeo.server$outputOGRFormats)) {
      return(error(res,400,paste("Format '",format,"' is not supported or recognized by GDAL or OGR",sep="")))
    }
    
    process_graph = .createSimpleArgList(process_graph)
  } else {
    return(error(res,400,"No process graph specified."))
    
  }
  
  job = openeo.server$createJob(user = req$user, process_graph = process_graph, storeProcessGraph=FALSE)

  job$loadProcessGraph()
  result = job$run()
  
  return(.create_output(res = res,result = result, format = format))

  
}

.ogrExtension = function(format) {
  metadata <- system(paste("ogrinfo --format ","\"",format,"\"",sep=""), intern=TRUE)
  
  extension.line = sapply(metadata, function(line) {
    m = regexec("\\s*Extension[s]?:\\s*([a-zA-Z0-9\\-_]*).*$",line)
    split = unlist(regmatches(line, m))
    if (length(split) > 0) {
      return(split[2])
    } else {
      return(NULL)
    }
  },USE.NAMES = FALSE)
  
  return(unlist(extension.line))
}

.create_output_no_response = function(result, format, dir) {
  #store the job? even though it is completed?
  if (!isCollection(result)) {
    cat("Creating vector file with OGR\n")
    # assuming that we don't have a collection as a result
    layername = 1 # TODO change to something meaningful
    extension = .ogrExtension(format)
    
    filename = paste(dir,"/output.",extension,sep="")

    cat(paste("storing file at",filename,"\n"))
    writeOGR(result,dsn=filename,layer=layername,driver=format)
    
  } else {
    cat("Creating raster file with GDAL\n")
    rasterdata = result$granules[[1]]$data #TODO handle multi granules...
    cat(str(rasterdata))
    

    filename = paste(dir,"output",sep="/")

    
    cat(paste("storing file at",filename,"\n"))
    rasterfile = writeRaster(x=rasterdata,filename=filename,format=format)
  }
}

.create_output = function(res, result, format) {
  #store the job? even though it is completed?
  if (!isCollection(result)) {
    cat("Creating vector file with OGR\n")
    # assuming that we don't have a collection as a result
    layername = 1 # TODO change to something meaningful

    filename = tempfile()

    cat(paste("storing file at",filename,"\n"))
    writeOGR(result,dsn=filename,layer=layername,driver=format)
    

    tryCatch({
      sendFile(res, 
               status=200, 
               file.name="output", 
               contentType=paste("application/ogr-",format,sep=""),
               data=readBin(filename, "raw", n=file.info(filename)$size))
    },finally = function(filename) {
      unlink(filename)
    })

    
  } else {
    cat("Creating raster file with GDAL\n")
    rasterdata = result$granules[[1]]$data #TODO handle multi granules...
    cat(str(rasterdata))
    
    cat(paste("storing file at",filename,"\n"))
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
# pipeline filter ====
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

.cors_option_bypass = function(req,res, ...) {
  res$setHeader("Access-Control-Allow-Headers", "Authorization, Accept, Content-Type")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH")
  
  ok(res)
}

.replace_user_me_in_body = function(req, res, ...) {
  if (!is.null(req$postBody)) {
    text = req$postBody
    text = gsub("users/me/files",paste("users",req$user$user_id,"files",sep="/"), text)
    req$postBody = text
  }
  forward()
}

.not_implemented_yet = function(req,res, ...) {
  error(res,501, "Not implemented, yet")
}


#
# utility functions ====
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
    res$setHeader("Content-Disposition", paste("attachment; filename=",file.name,file.ext,sep=""))
  }
  return(res)
}

.isURL = function(url) {
  pattern= "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$"
  return(grepl(pattern,url))
}


#
# setup the routes ====
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
  
  
  root$handle("GET",
              "/api/capabilities/output_formats",
              handler = .output_formats,
              serializer = serializer_unboxed_json())
  
  root$handle("OPTIONS",
              "/api/capabilities/output_formats",
              handler = .cors_option_bypass)
  
  root$handle("GET",
              "/api/capabilities/services",
              handler = .services,
              serializer = serializer_unboxed_json())
  root$handle("OPTIONS",
              "/api/capabilities/services",
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
  
  
  executeSynchronous = plumber$new()
  executeSynchronous$handle("POST",
                            "/",
                            handler = .executeSynchronous,
                            serializer = serializer_unboxed_json())
  executeSynchronous$handle("OPTIONS",
                            "/",
                            handler = .cors_option_bypass)
  executeSynchronous$filter("authorization",.authorized)
  executeSynchronous$filter("me_filter",.replace_user_me_in_body)
  
  root$mount("/api/execute",executeSynchronous)
  
  services = createServicesEndpoint()
  root$mount("/api/services",services)
  
  wms = createWMSEndpoint()
  root$mount("/api/wms",wms)
  
  udf_runtimes = createUDFRuntimesEndpoint()
  root$mount("/api/udf_runtimes",udf_runtimes)
  
  return(root)
}