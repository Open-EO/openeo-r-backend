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
      openEO.R.Backend:::error(res,403,"Login failed.")
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
    
    if (is.null(format) || 
        !(format %in% openeo.server$outputGDALFormats || 
          format %in% openeo.server$outputOGRFormats)) {
      return(error(res,400,paste("Format '",format,"' is not supported or recognized by GDAL or OGR",sep="")))
    }
    
    process_graph = .createSimpleArgList(process_graph)
  } else {
    return(error(res,400,"No process graph specified."))
    
  }

  tryCatch({
    job = Job$new(process_graph=process_graph,user_id = req$user$user_id)
    job$job_id = syncJobId()
    
    job = job$run()
    
    if (is.null(job$results)) {
      return(openEO.R.Backend:::error(res,status = 500,msg = "The result was NULL due to an internal error during processing."))
    }
    
    return(.create_output(res = res,result = job$results, format = format))
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
.create_output_no_response = function(result, format, dir) {
    result$toFile(dir,format=format)
}



# creates file output for a direct webservice result (executeSynchronous)
.create_output = function(res, result, format) {
  #store the job? even though it is completed?
  if (result$dimensions$feature) {
    contentType = paste("application/x-ogr-",format,sep="")
  } else {
    contentType = paste("application/x-gdal-",format,sep="")
  }
    
  tryCatch({
    temp = result$toFile(format=format, temp=TRUE)
    first = temp$getData()$output.file[[1]]
    sendFile(res, 
             status=200, 
             file.name="output", 
             contentType=contentType,
             data=readBin(first, "raw", n=file.info(first)$size))
  },error=function(e){
    openEO.R.Backend:::error(res,500,e)
  },finally = {
    unlink(temp$getData()$output.file)
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
  
  wfs = createWFSEndpoint()
  root$mount("/api/wfs",wfs)
  
  udf_runtimes = createUDFRuntimesEndpoint()
  root$mount("/api/udf_runtimes",udf_runtimes)
  
  return(root)
}