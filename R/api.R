# 
# need to use source inclusion at the api level, since plumber
# loads this script and creates the api. any functions / variables
# or else defined at package level is not visible here elsewise.
# 
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
#' @include api_credentials.R
#' @include api_process_graphs.R
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
  endpoints = openeo.server$getEndpoints()
  
  endpoints = endpoints %>% group_by(path) %>% summarise(
    path_capabilities=list(tibble(path,method) %>% (function(x,...){
      return(list(path=unique(x$path),method=x$method))
    }))
  )
  
  list(
    version = openeo.server$api.version,
    endpoints = endpoints$path_capabilities,
    billing = list(
      currency = "EUR",
      plans = list(
        list(
          name="free",
          description = "Free. Unlimited calculations, no credit use. Its a test system!",
          url="http://openeo.org/plans/free-plan"
        )
      )
    )
  )
}

.output_formats = function() {
  formats = c(openeo.server$outputGDALFormats,openeo.server$outputOGRFormats)
  namedList = lapply(formats,function(format) {
    res = list()
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
    WMS = list(
      arguments=list(
        version=list(
          type="string",
          description="The WMS version that has to be used.",
          default="1.3.0",
          enum=c("1.1.1","1.3.0")
        )
      ),
      attributes = list(
        layers=list(
          type="array",
          description="Array of layer names.",
          example=c("b01","b02","ndvi")
        )
      )
    )
  ))
}


#
# download endpoint ====
#

.executeSynchronous = function(req,res,format=NULL) {

  if (!is.null(req$postBody)) {
    sent_job = fromJSON(req$postBody,simplifyDataFrame = FALSE)
    output = sent_job$output
    process_graph = sent_job$process_graph
    
    if (is.null(format)) {
      format = output$format
    } 
    
    if (is.null(format) || 
        !(format %in% openeo.server$outputGDALFormats || 
          format %in% openeo.server$outputOGRFormats)) {
      return(error(res,400,paste("Format '",format,"' is not supported or recognized by GDAL or OGR",sep="")))
    }
    
  } else {
    return(error(res,400,"No process graph specified."))
    
  }

  tryCatch({
    job = Job$new(process_graph=process_graph,user_id = req$user$user_id)
    job$output = output
    job$job_id = syncJobId()
    
    openeo.server$runJob(job = job, format = format, response = TRUE, res = res)
    
    job$clearLog()
    return(res)
    
  }, error= function(e) {
    return(openEO.R.Backend:::error(res=res, status = 500, msg = e))
  }, finally = {
    removeJobsUdfData(job)
  })
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

# creates files for batch processing
.create_output_no_response = function(result, format, dir,logger) {
    result$toFile(dir,format=format,logger)
}



# creates file output for a direct webservice result (executeSynchronous)
.create_output = function(res, result, format, logger) {
  #store the job? even though it is completed?
  if (is.null(result)) {
    logger$error("Outputter did not receive a collection for output.")
  }
  
  if (result$dimensions$feature) {
    contentType = paste("application/x-ogr-",format,sep="")
  } else {
    contentType = paste("application/x-gdal-",format,sep="")
  }
    
  tryCatch({
    temp = result$toFile(format=format, temp=TRUE, logger=logger)
    first = temp$getData()$output.file[[1]]
    sendFile(res, 
             status=200, 
             file.name="output", 
             contentType=contentType,
             data=readBin(first, "raw", n=file.info(first)$size))
  },error=function(e){
    openEO.R.Backend:::error(res,500,e)
  },finally = {
    if (!is.null(temp)) {
      unlink(temp$getData()$output.file)
    }
  })
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
      
      user = User$new()
      user = user$load(user_id = user_id)
      req$user = user
      
      forward()

    } else {
      stop("Incorrect authentication method.")
    }
  },
  error = function(e) {
    openEO.R.Backend:::error(res,401,"Unauthorized")
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
  
  res$status = 200
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
  error(res,501, "The back-end responds with this error code whenever an endpoint is specified in the openEO API, but is not supported.", code = 501)
}


#
# utility functions ====
#

error = function(res, status,msg, code = NULL) {
  res$status = status
  
  # id and links are spared for now
  return(list(
    code = code,
    message=msg,
    links = list())
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
  
  openeo.server$registerEndpoint("/","GET")
  root$handle("GET",
              "/api/",
              handler = .capabilities,
              serializer = serializer_unboxed_json())
  root$handle("OPTIONS",
              "/api/",
              handler = .cors_option_bypass)
  
  openeo.server$registerEndpoint("/output_formats","GET")
  root$handle("GET",
              "/api/output_formats",
              handler = .output_formats,
              serializer = serializer_unboxed_json())
  root$handle("OPTIONS",
              "/api/output_formats",
              handler = .cors_option_bypass)
  
  openeo.server$registerEndpoint("/service_types","GET")
  root$handle("GET",
              "/api/service_types",
              handler = .services,
              serializer = serializer_unboxed_json())
  root$handle("OPTIONS",
              "/api/service_types",
              handler = .cors_option_bypass)
  
  
  root$handle("GET",
              "/api/subscription",
              handler = .not_implemented_yet)
  root$handle("OPTIONS",
              "/api/subscription",
              handler = .cors_option_bypass)
  
  root$handle("POST",
              "/api/validation",
              handler = .not_implemented_yet)
  root$handle("OPTIONS",
              "/api/validation",
              handler = .cors_option_bypass)
  
  root$registerHook("postroute",.cors_filter)
  
  credentials = createCredentialsEndpoint()
  root$mount("/api/credentials",credentials)
  
  data = createDataEndpoint()
  root$mount("/api/data",data)
  
  process = createProcessesEndpoint()
  root$mount("/api/processes",process)
  
  jobs = createJobsEndpoint()
  root$mount("/api/jobs",jobs)
  
  me = createMeEndpoint()
  root$mount("/api/me",me)
  
  files = createFilesEndpoint()
  root$mount("/api/files",files)
  
  process_graphs = createProcessGraphsEndpoint()
  root$mount("/api/process_graphs",process_graphs)
  
  openeo.server$registerEndpoint(path = "/preview/", "POST")
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
  
  root$mount("/api/preview",executeSynchronous)
  
  services = createServicesEndpoint()
  root$mount("/api/services",services)
  
  # additional apis to handle and forward OGC service request
  wms = createWMSEndpoint()
  root$mount("/api/wms",wms)
  
  wfs = createWFSEndpoint()
  root$mount("/api/wfs",wfs)
  
  udf_runtimes = createUDFRuntimesEndpoint()
  root$mount("/api/udf_runtimes",udf_runtimes)
  
  return(root)
}