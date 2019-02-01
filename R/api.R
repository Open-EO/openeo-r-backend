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
#' @include api_processes.R
#' @include api_credentials.R
#' @include api_process_graphs.R
#' @include FilterableEndpoint.R
#' @include errors.R
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

.capabilities = function() {
  endpoints = openeo.server$getEndpoints()
  
  endpoints = endpoints %>% group_by(path) %>% summarise(
    path_capabilities=list(tibble(path,method) %>% (function(x,...){
      return(list(path=unique(x$path),methods=as.list(x$method)))
    }))
  )
  
  list(
    version = openeo.server$configuration$api.version,
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
  raster.only.formats = openeo.server$configuration$outputGDALFormats[which(! openeo.server$configuration$outputGDALFormats %in% openeo.server$configuration$outputOGRFormats)]
  rasterformats = lapply(raster.only.formats, function(formatname) {
    format = list(
      gis_data_types = c("raster")
    )
    return(format)
  })
  names(rasterformats) = raster.only.formats
  
  vector.only.formats = openeo.server$configuration$outputOGRFormats[which(! openeo.server$configuration$outputOGRFormats %in% openeo.server$configuration$outputGDALFormats)]
  vectorformats = lapply(vector.only.formats, function(formatname) {
    format = list(
      gis_data_types = c("vector")
    )

    return(format)
  })
  names(vectorformats) = vector.only.formats
  
  
  both.type.formats = openeo.server$configuration$outputOGRFormats[which(openeo.server$configuration$outputOGRFormats %in% openeo.server$configuration$outputGDALFormats)]
  bothformats = lapply(both.type.formats, function(formatname) {
    format = list(
      gis_data_types = c("raster","vector"),
      parameters = list(gis_data_type = list(
          type = "string",
          enum = c("raster","vector"),
          required = TRUE
        )
      )
    )
    return(format)
  })
  names(bothformats) = both.type.formats
  
  formats = c(rasterformats,vectorformats,bothformats)
  
  return(list(
    default="GTiff",
    formats = formats
  ))
}

.services = function() {
  # TODO also list WFS and WCS here
  return(list(
    WMS = list(
      parameters=list(
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

.udf_runtimes = function() {
  return(lapply(openeo.server$udf_runtimes, function(x){
    x=unclass(x)
    x$performTransaction = NULL
    return(x)
  } ))
}


# /preview endpoint ----
.executeSynchronous = function(req,res,format=NULL) {
  tryCatch({
    if (!is.null(req$postBody)) {
      sent_job = fromJSON(req$postBody,simplifyDataFrame = FALSE)
      output = sent_job$output
      process_graph = sent_job$process_graph
      
      if (is.null(format)) {
        format = output$format
      } 
      
      if (is.null(format) || 
          !(format %in% openeo.server$configuration$outputGDALFormats || 
            format %in% openeo.server$configuration$outputOGRFormats)) {
        throwError("FormatUnsupported")
      }
      
    } else {
      throwError("ContentTypeInvalid",types="application/json")
      
    }
    
    job = Job$new(process_graph=process_graph,user_id = req$user$user_id)
    job$output = output
    job$job_id = syncJobId()
    
    openeo.server$runJob(job = job, format = format, response = TRUE, res = res)
    
    job$clearLog()
    return(res)

      
  
  },error=handleError,
  finally = {
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
    result$toFile(dir=dir,format=format,logger=logger)
}



# creates file output for a direct webservice result (executeSynchronous)
.create_output = function(res, result, format, logger) {
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
    logger$error(e$message)
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
  tryCatch({
    if (req$REQUEST_METHOD == 'OPTIONS') {
      return(forward())
    }

    auth = unlist(strsplit(req$HTTP_AUTHORIZATION," "))
    if (auth[1] == "Bearer") {
      token = auth[2]
      hextoken = hex2bin(token)
      nonce.length = 24
      msg = hextoken[1:(length(hextoken)-nonce.length)]
      nonce = hextoken[((length(hextoken)-nonce.length)+1):length(hextoken)]
      
      user_id = rawToChar(data_decrypt(msg,openeo.server$configuration$secret.key,nonce))
      
      user = User$new()
      user = user$load(user_id = user_id)
      req$user = user
      
      forward()
      
    } else {
      throwError("AuthenticationSchemeInvalid")
    }

  }, error=handleError)
  
}

.validateProcessGraphFilter = function(req, res, ...) {
  tryCatch({
    parsedGraph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
    
    is_process_graph_set = "process_graph" %in% names(parsedGraph)
    
    if (is_process_graph_set) {
      process_graph = ProcessGraph$new(process_graph = parsedGraph[["process_graph"]])
    } else {
      if (req$REQUEST_METHOD == "POST") {
        throwError("ProcessGraphMissing")
      } else {
        # the other option is PATCH, but there we don't require process_graph
        forward()
      }
    }
    
    
    
    tryCatch({
      process_graph$buildExecutableProcessGraph(user = req$user)
      forward()
      
      
    },error=function(e) {
      # TODO improve this further
      throwError("ProcessGraphMissing")
    }) 
  }, error=handleError)
  
}

.cors_filter = function(req,res) {
  res$setHeader("Access-Control-Allow-Origin", req$HTTP_ORIGIN)
  res$setHeader("Access-Control-Allow-Credentials", "true")
  res$setHeader("Access-Control-Expose-Headers", "OpenEO-Identifier, OpenEO-Costs")
  forward()
}

.cors_option_bypass = function(req,res, ...) {
  res$setHeader("Access-Control-Allow-Headers", "Authorization, Accept, Content-Type")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH")
  
  res$status = 200
}

.not_implemented_yet = function(req,res, ...) {
  error(res,501, "The back-end responds with this error code whenever an endpoint is specified in the openEO API, but is not supported.", code = 501)
}


#
# utility functions ====
#

# @deprecated in the next time
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
  
  AuthFilter = openeo.server$createFilter("authorization",.authorized)
  ProcessGraphValidationFilter = openeo.server$createFilter("pg_validator",.validateProcessGraphFilter)
  
  # serializer default is serializer_unboxed_json() if serializer is omitted
  
  # server - capabilities ====
  openeo.server$registerEndpoint(path = "/",
                                 method = "GET",
                                 handler = .capabilities)

  # server - output formats ====
  openeo.server$registerEndpoint(path="/output_formats",
                                 method="GET",
                                 handler = .output_formats)
  
  # server - service types ====
  openeo.server$registerEndpoint(path="/service_types",
                                 method="GET",
                                 handler=.services)
  
  # server - udf runtimes ====
  openeo.server$registerEndpoint(path="/udf_runtimes",
                                 method="GET",
                                 handler=.udf_runtimes)

  # server - subscription ====
  openeo.server$registerEndpoint(path="/subscription",
                                 method = "GET",
                                 unsupported = TRUE)

  # server - validation ====
  openeo.server$registerEndpoint(path="/validation",
                                 method="POST",
                                 handler=.validateProcessGraph)

  # credentials - basic ====
  openeo.server$registerEndpoint(path="/credentials/basic",
                                 method="GET",
                                 handler = .login_basic)

  # credentials - oidc ====
  # this is just for testing, there is no actual oidc integration for this back-end
  # openeo.server$registerEndpoint(path="/credentials/oidc",
  #                                method="GET",
  #                                handler=.login_oidc)
  openeo.server$registerEndpoint(path="/credentials/oidc",
                                 method="GET",
                                 unsupported = TRUE)

  # collections - list all ====
  openeo.server$registerEndpoint(path="/collections",
                                 method="GET",
                                 handler=.listData)
  
  # collections - describe data ====
  openeo.server$registerEndpoint(path="/collections/{name}",
                                 method="GET",
                                 handler=.describeData)

  # processes - list all ====
  openeo.server$registerEndpoint(path="/processes",
                                 method="GET",
                                 handler=.listProcesses)
  
  # me - user information ====
  openeo.server$registerEndpoint(path = "/me",
                                 method="GET",
                                 filters = list(AuthFilter),
                                 handler = .userInformation)
  
  # jobs - list all ====
  openeo.server$registerEndpoint(path="/jobs",
                                 method="GET",
                                 handler=.listUserJobs,
                                 filters = list(AuthFilter))
  
  # jobs - create new ====
  openeo.server$registerEndpoint(path = "/jobs",
                                 method = "POST",
                                 handler = .createNewJob,
                                 filters = list(AuthFilter,
                                                ProcessGraphValidationFilter))

  # jobs - describe ====
  openeo.server$registerEndpoint(path = "/jobs/{job_id}",
                                 method = "GET",
                                 handler = .describeJob,
                                 filters = list(AuthFilter))

  # jobs - update ====
  openeo.server$registerEndpoint(path = "/jobs/{job_id}",
                                 method = "PATCH",
                                 handler = .updateJob,
                                 filters = list(AuthFilter,
                                                ProcessGraphValidationFilter))

  # jobs - delete ====
  openeo.server$registerEndpoint(path = "/jobs/{job_id}",
                                 method = "DELETE",
                                 handler = .deleteJob,
                                 filters = list(AuthFilter))
  
  # jobs - perform async ====
  openeo.server$registerEndpoint(path = "/jobs/{job_id}/results",
                                 method = "POST",
                                 handler = .performJob,
                                 filters = list(AuthFilter))
  
  # jobs - describe job results ====
  openeo.server$registerEndpoint(path = "/jobs/{job_id}/results",
                                 method = "GET",
                                 handler = .createDownloadableFileList,
                                 filters = list(AuthFilter))

  # jobs - cost estimation ====
  openeo.server$registerEndpoint(path = "/jobs/{job_id}/estimate",
                                 method = "GET",
                                 handler = .estimateJobCosts,
                                 filters = list(AuthFilter))
  
  # files - list all ====
  openeo.server$registerEndpoint(path = "/files/{user_id}",
                                 method = "GET",
                                 handler = .listUserFiles,
                                 filters = list(AuthFilter))
  
  # files - download file ====
  openeo.server$registerEndpoint(path = "/files/{user_id}/{path}",
                                 method = "GET",
                                 handler = .downloadUserFile,
                                 filters = list(AuthFilter))
  
  # files - upload file ====
  openeo.server$registerEndpoint(path = "/files/{user_id}/{path}",
                                 method = "PUT",
                                 handler = .uploadFile,
                                 filters = list(AuthFilter)) # think about a new filter that handles the URL encoding

  # files - delete file ====
  openeo.server$registerEndpoint(path = "/files/{user_id}/{path}",
                                 method = "DELETE",
                                 handler = .deleteUserFile,
                                 filters = list(AuthFilter))

  # graphs - create ====
  openeo.server$registerEndpoint(path = "/process_graphs",
                                 method = "POST",
                                 handler = .createProcessGraph,
                                 filters = list(AuthFilter,
                                                ProcessGraphValidationFilter))

  # graphs - list all ====
  openeo.server$registerEndpoint(path = "/process_graphs",
                                 method = "GET",
                                 handler = .listUserProcessGraphs,
                                 filters = list(AuthFilter))
  
  # graphs - delete ====
  openeo.server$registerEndpoint(path = "/process_graphs/{process_graph_id}",
                                 method = "DELETE",
                                 handler = .deleteProcessGraph,
                                 filters = list(AuthFilter))
  
  # graphs - describe ====
  openeo.server$registerEndpoint(path = "/process_graphs/{process_graph_id}",
                                 method = "GET",
                                 handler = .getProcessGraph,
                                 filters = list(AuthFilter))
  
  # graphs - update ====
  openeo.server$registerEndpoint(path = "/process_graphs/{process_graph_id}",
                                 method = "PATCH",
                                 handler = .modifyProcessGraph,
                                 filters = list(AuthFilter,
                                                ProcessGraphValidationFilter))

  # server - preview ====
  openeo.server$registerEndpoint(path = "/preview", 
                                 method = "POST",
                                 handler = .executeSynchronous,
                                 filters = list(AuthFilter,
                                                ProcessGraphValidationFilter))
  
  # services - create ====
  openeo.server$registerEndpoint(path = "/services",
                                 method = "POST",
                                 handler = .createNewService,
                                 filters = list(AuthFilter,
                                                ProcessGraphValidationFilter))
  
  # services - list all ====
  openeo.server$registerEndpoint(path = "/services",
                                 method = "GET",
                                 handler = .listUserServices,
                                 filters = list(AuthFilter))

  # services - describe ====
  openeo.server$registerEndpoint(path = "/services/{service_id}",
                                 method = "GET",
                                 handler = .getServiceInformation,
                                 filters = list(AuthFilter))

  # services - delete ====
  openeo.server$registerEndpoint(path = "/services/{service_id}",
                                 method = "DELETE",
                                 handler = .deleteService,
                                 filters = list(AuthFilter))

  # services - update ====
  openeo.server$registerEndpoint(path = "/services/{service_id}",
                                 method = "PATCH",
                                 handler = .updateService,
                                 filters = list(AuthFilter,
                                                ProcessGraphValidationFilter))

  # wms - referer ====
  openeo.server$registerEndpoint(path = "/wms/{service_id}",
                                 method = "GET",
                                 handler = .referToMapserver,
                                 serializer = serializer_proxy(),
                                 unsupported = TRUE)
  
  # wfs - referer ====
  openeo.server$registerEndpoint(path = "/wfs/{service_id}",
                                 method = "GET",
                                 handler = .referToMapserver,
                                 serializer = serializer_proxy(),
                                 unsupported = TRUE)

  # server - udf runtimes ====
  openeo.server$registerEndpoint(path = "/udf_runtimes",
                                 method = "GET",
                                 unsupported = TRUE)
  
  # server - describe udf runtime
  openeo.server$registerEndpoint(path = "/udf_runtimes/{lang}/{udf_type}",
                                 method = "GET",
                                 unsupported = TRUE)

  invisible(TRUE)
}

forward = function() {
  openeo.globals$forwarded=TRUE
}