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
  
  openeo.server$registerEndpoint("/services/","GET")
  services$handle("GET",
               "/",
               handler = .listUserServices,
               serializer = serializer_unboxed_json())
  services$handle("OPTIONS",
               "/",
               handler = .cors_option_bypass)
  
  openeo.server$registerEndpoint("/services/{service_id}","GET")
  services$handle("GET",
                  "/<service_id>",
                  handler=.getServiceInformation,
                  serializer = serializer_unboxed_json())
  
  openeo.server$registerEndpoint("/services/{service_id}","DELETE")
  services$handle("DELETE",
                  "/<service_id>",
                  handler=.deleteService,
                  serializer = serializer_unboxed_json())
  
  openeo.server$registerEndpoint("/services/{service_id}","PATCH")
  services$handle("PATCH",
                  "/<service_id>",
                  handler=.updateService,
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
  
  .createNewServiceJob(service = service, user_id = req$user$user_id, process_graph = service_input$process_graph)
  
  
  #if not running or finished then run job!
  .runServiceJob(service = service)

  # TODO wait until finished before proceeding
  
  # when finished then create: create map file
  service$buildMapFile()
  
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
    error(res, 404, "Cannot find service")
  }
}

.updateService = function(req,res,service_id) {
  if (exists.Service(service_id)) {
    patch = fromJSON(req$postBody,simplifyDataFrame=FALSE)
    
    service = Service$new(service_id)$load()
    
    rebuildMapFile = FALSE
    rerunJob = FALSE
    
    if (!is.null(patch$title)) {
      service$title = patch$title
    }
    
    if (!is.null(patch$description)) {
      service$description = patch$description
    }
    
    if (!is.null(patch$process_graph)) {
      job = service$job
      pg = job$getProcessGraph()
      pg$process_graph = patch$process_graph
      pg$update()
      
      job$status = "submitted"
      job$last_update = as.character(now())
      
      job$store()
      
      rerunJob=TRUE
      rebuildMapFile = TRUE
    }
    
    if (!is.null(patch$enabled)) {
      service$enabled = patch$enabled
    }
    
    if (!is.null(patch$parameters)) {
      service$parameters = patch$parameters
      rebuildMapFile = TRUE
    } else {
      if ("parameters" %in% names(patch)) {
        service$parameters = NA
      }
    }
    
    if (!is.null(patch$plan)) {
      service$plan = patch$plan
    }
    
    if (!is.null(patch$budget)) {
      service$budget = patch$budget
    }
    
    service$store()
    
    if (rerunJob) {
      .runServiceJob(service)
    }
    
    if (rebuildMapFile) {
      service$buildMapFile()
    }
    
    res$status = 204
  } else {
    error(res, 404, "Cannot find service")
  }
}

.deleteService = function(req,res,service_id) {
  if (exists.Service(service_id)) {
    service = Service$new(service_id)$load()
    
    service$remove()
    
    res$status = 204
  } else {
    error(res, 404, "Cannot find service")
  }
}

.listUserServices = function(req,res) {
  userid = req$user$user_id
  return(
    lapply(req$user$services, function(service_id) {
      return(Service$new(service_id)$load()$shortInfo())
    })
  )
}

.runServiceJob = function(service) {
  job = service$job
  
  if (dir.exists(job$output.folder)) {
    file.remove(list.files(job$output.folder, full.names = TRUE))
  }
  
  # TODO consider also that the result is a feature
  if (job$status %in% c("submitted","error")) {
    if (is.null(job$output) || is.null(job$output[["format"]])) {
      format = "GTiff"
    } else {
      format = job$output[["format"]]
    }
    openeo.server$runJob(job=job, format = format,response=FALSE)
  }
}

.createNewServiceJob = function(service, user_id, process_graph) {
  job = Job$new(user_id = user_id, process_graph = process_graph)
  job$store()
  service$job_id = job$job_id
  
  service$store()
  
  job$load()
  
  # also set a title that is assigned to a service
  job$title = paste("Service",service$service_id)
  job$status = "submitted"
  job$store()
}