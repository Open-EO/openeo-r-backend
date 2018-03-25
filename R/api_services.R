# services endpoints ----
createServicesEndpoint = function() {
  services = plumber$new()
  
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
  service_input = fromJSON(req$postBody,simplifyDataFrame = FALSE)
  
  service = Service$new()
  type = tolower(service_input$service_type)
  if (is.null(type)) {
    return(error(res, 400, "No service type specified"))
  } else {
    service$service_type = type
  }
  
  
  args = service_input$service_args
  service$service_args = args
  
  job_id = service_input$job_id
  if (!is.null(job_id) && openeo.server$jobExists(job_id)) {
    service$job_id = job_id
  } else {
    return(error(res,500,"Cannot link to job. Please create a job first."))
  }
  
  service$store()
  
  job = openeo.server$loadJob(job_id) 
  #if not running or finished then run job!
  
  # when finished then create: create map file
  if (type %in% c("wms","wcs")) {
    files = list.files(paste(openeo.server$workspaces.path,"jobs",job_id,sep="/"),pattern="[^process.log|map.map]",full.names = TRUE)

    config = MapServerConfig$new()
    config = config$fromRaster(obj = raster(files[1]),service=service)
    #TODO maybe we need to create layers for each additional file...
    
    mapfile = paste(openeo.server$workspaces.path,"services",paste(service$service_id,".map",sep=""),sep="/")
    config$toFile(mapfile)
  } else {
    #TODO
    job_folder = paste(openeo.server$workspaces.path,"jobs",job_id,sep="/")
    files = list.files(job_folder,pattern="*.shp",full.names = TRUE)
    config = MapServerConfig$new()
    config = config$fromVector(obj = readOGR(files[1]),service=service,data.dir=job_folder)
    
    mapfile = paste(openeo.server$workspaces.path,"services",paste(service$service_id,".map",sep=""),sep="/")
    config$toFile(mapfile)
  }
  
  return(service$detailedInfo())
}

.referToMapserver = function(req,res,service_id) {
  s = req$QUERY_STRING
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
  if (exists.service(service_id)) {
    service = Service$new()$load(service_id)
    
    return(service$detailedInfo())
  } else {
    error(re, 404, "Cannot find service")
  }
}

.updateService = function(req,res,service_id) {
  if (exists.service(service_id)) {
    patch = fromJSON(req$postBody,simplifyDataFrame=FALSE)
    if (names(patch) == "service_args") {
      args = patch[["service_args"]]
      service = Service$new()$load(service_id)
      
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
  if (exists.service(service_id)) {
    con = openeo.server$getConnection()
    deleteQuery = "delete from service where service_id = :sid"
    dbExecute(con, deleteQuery, param=list(sid=service_id))
    dbDisconnect(con)
    
    mapfile = paste(openeo.server$workspaces.path,"services",paste(service_id,"map",sep="."),sep="/")
    
    if (file.exists(mapfile)) {
      unlink(mapfile)
    }
    
    ok(res)
  } else {
    error(re, 404, "Cannot find service")
  }
}