# /services handler functions ----

.createNewService = function(req, res) {
  tryCatch({
    if (length(req$postBody) == 0 || is.null(req$postBody) || is.na(req$postBody)) {
      throwError("ContentTypeInvalid",types="application/json")
    }
    
    service_input = fromJSON(req$postBody,simplifyDataFrame = FALSE)
    
    if (is.null(service_input$process_graph)) {
      throwError("ServiceArgumentRequired",argument="process_graph")  
    }
    
    if ( is.null(service_input$type)) {
      throwError("ServiceArgumentRequired",argument="type")
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
    res$setHeader(name = "OpenEO-Identifier",value=service$service_id)
    res$status = 201
    
    return(res)
  },error=handleError)
  
}

.referToMapserver = function(req,res,service_id) {
  tryCatch({
    if (!exists.Service(service_id)) {
      throwError("ServiceNotFound")
    }
    
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
    #TODO eventually use 303 redirect with Location header
    return(response)
  },error=handleError)
  
}

.getServiceInformation = function(req,res,service_id) {
  tryCatch({
    if (exists.Service(service_id)) {
      service = Service$new(service_id)$load()
      
      return(service$detailedInfo())
    } else {
      throwError("ServiceNotFound")
    }
  },error=handleError)
  
}

.updateService = function(req,res,service_id) {
  tryCatch({
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
        
        update_pg = ProcessGraph$new()
        update_pg$graph_id = pg$graph_id 
        update_pg$user_id = pg$user_id
        update_pg$process_graph = patch$process_graph
        update_pg$update()
        
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
      
      return(res)
    } else {
      throwError("ServiceNotFound")
    }
  }, error=handleError)
  
}

.deleteService = function(req,res,service_id) {
  tryCatch({
    if (exists.Service(service_id)) {
      # TODO check user_id also
      service = Service$new(service_id)$load()
      
      service$remove()
      
      res$status = 204
      
      return(res)
    } else {
      throwError("ServiceNotFound")
    }
  }, error=handleError)
  
}

.listUserServices = function(req,res) {
  tryCatch({
    return(list(
      services=lapply(req$user$services, function(service_id) {
        return(Service$new(service_id)$load()$shortInfo())
      }),
      links=list()
    ))
  }, error=handleError)
  
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