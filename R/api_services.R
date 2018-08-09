# services endpoints ----
createServicesEndpoint = function() {
  services = plumber$new()
  
  # create new service ====
  openeo.server$registerEndpoint("/services/","POST")
  services$handle("POST",
                  "/",
                  handler=.createNewService,
                  serializer = serializer_unboxed_json())
  services$handle("OPTIONS",
                  "/",
                  handler=.cors_option_bypass)
  
  services$handle("GET",
                  "/<service_id>",
                  handler=.getServiceInformation,
                  serializer = serializer_unboxed_json())
  services$handle("PATCH",
                  "/<service_id>",
                  handler=.updateService,
                  serializer = serializer_unboxed_json())
  services$handle("DELETE",
                  "/<service_id>",
                  handler=.deleteService,
                  serializer = serializer_unboxed_json())
  services$handle("OPTIONS",
                  "/<service_id>",
                  handler=.cors_option_bypass)
  
  services$filter("authorization",.authorized)
  
  return(services)
}

# redirecter endpoints ----
createWMSEndpoint = function() {
  wms = plumber$new()
  
  wms$handle("GET",
                  "/<service_id>",
                  handler=.referToMapserver,
                  serializer = serializer_proxy())
  
  wms$handle("OPTIONS",
                  "/<service_id>",
                  handler=.cors_option_bypass)
  
  return(wms)
}

createWFSEndpoint = function() {
  wfs = plumber$new()
  
  wfs$handle("GET",
             "/<service_id>",
             handler=.referToMapserver,
             serializer = serializer_proxy())
  
  wfs$handle("OPTIONS",
             "/<service_id>",
             handler=.cors_option_bypass)
  
  return(wfs)
}

# handler ----
.createNewService = function(req, res) {
  if (length(req$postBody) == 0 || is.null(req$postBody) || is.na(req$postBody)) {
    return(error(res,400, "No Request Body"))
  }
  
  service_input = fromJSON(req$postBody,simplifyDataFrame = FALSE)
  
  if (is.null(service_input$process_graph) || is.null(service_input$type)) {
    return(error(res,400, "Either process_graph or service type is missing."))
  }
  
  service = Service$new()

  service$type = tolower(service_input$type)
  
  if (!is.null(service_input$title)) {
    service$title = service_input$title
  }
  
  if (!is.null(service_input$description)) {
    service$description = service_input$description
  }
  
  if (!is.null(service_input$enabled)) {
    service$enabled = service_input$enabled
  }
  
  if (!is.null(service_input$plan)) {
    service$plan = service_input$plan
  }
  
  if (!is.null(service_input$budget)) {
    service$budget = service_input$budget
  }
 
  params = service_input$parameters
  if (!is.null(params)) {
    service$parameters = params
  }
  
  job = Job$new(user_id = req$user$user_id, process_graph = service_input$process_graph)
  job$store()
  service$job_id = job$job_id
  
  service$store()
  
  job$load()
  
  # also set a title that is assigned to a service
  job$title = paste("Service",service$service_id)
  job$store()
  
  
  #if not running or finished then run job!
  # TODO consider also that the result is a feature
  if (job$status %in% c("submitted","error")) {
    if (is.null(job$output) || is.null(job$output[["format"]])) {
      format = "GTiff"
    } else {
      format = job$output[["format"]]
    }
    openeo.server$runJob(job=job, format = format)
  }
  # TODO wait until finished before proceeding
  
  # when finished then create: create map file
  if (service$type %in% c("wms","wcs")) {
    job_result_path = paste(openeo.server$workspaces.path,"jobs",job$job_id,sep="/")
    files = list.files(job_result_path,pattern="[^process.log|map.map]",full.names = TRUE)
    files = setdiff(files,list.files(job_result_path,pattern="aux.xml",full.names = TRUE))
    config = MapServerConfig$new()
    config = config$fromRaster(obj = lapply(files,brick),service=service)
    #TODO maybe we need to create layers for each additional file...

    mapfile = paste(openeo.server$workspaces.path,"services",paste(service$service_id,".map",sep=""),sep="/")
    config$toFile(mapfile)
  } else {
    job_folder = paste(openeo.server$workspaces.path,"jobs",job$job_id,sep="/")
    files = list.files(job_folder,pattern="*.shp",full.names = TRUE)
    if (is.null(files) || length(files) == 0) {
      stop("Cannot find SHP file to create WFS from.")
    }
    
    config = MapServerConfig$new()
    config = config$fromVector(obj = readOGR(files[1]),service=service,data.dir=job_folder)
    
    mapfile = paste(openeo.server$workspaces.path,"services",paste(service$service_id,".map",sep=""),sep="/")
    config$toFile(mapfile)
  }
  
  res$setHeader("Location",paste(openeo.server$baseserver.url,"services/",service$service_id))
  res$status = 201
}

.referToMapserver = function(req,res,service_id) {
  s = URLdecode(req$QUERY_STRING)
  queryKVP = unlist(strsplit(substr(s,2,nchar(s)),"[&=]"))
  keys = "map"
  values = list(paste("/maps/services/",service_id,".map",sep=""))
  
  for (key_index in seq(1,length(queryKVP),2)) {
    key = queryKVP[key_index]
    value = queryKVP[key_index+1]
    
    keys = append(keys,key)
    values = append(values,value)
  }
  names(values) = keys
  
  
  url = openeo.server$mapserver.url
  if (endsWith(url, "?")) {
    url = substr(url, 1, nchar(url)-1)
  }
  response = GET(url = url,query = values)
}

.getServiceInformation = function(req,res,service_id) {
  if (exists.Service(service_id)) {
    service = Service$new(service_id)$load()
    
    return(service$detailedInfo())
  } else {
    error(re, 404, "Cannot find service")
  }
}

.updateService = function(req,res,service_id) {
  if (exists.Service(service_id)) {
    patch = fromJSON(req$postBody,simplifyDataFrame=FALSE)
    if (names(patch) == "service_args") {
      args = patch[["service_args"]]
      service = Service$new(service_id)$load()
      
      for (key in names(args)) {
        value = args[[key]]
        service$service_args[[key]] = value
      }
      
      service$store()
      ok(res)
    }
    
  } else {
    error(res, 404, "Cannot find service")
  }
}

.deleteService = function(req,res,service_id) {
  if (exists.Service(service_id)) {
    service = Service$new(service_id)
    service$remove()
    
    ok(res)
  } else {
    error(re, 404, "Cannot find service")
  }
}