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
                  handler=.not_implemented_yet,
                  serializer = serializer_unboxed_json())
  services$handle("PATCH",
                  "/<service_id>",
                  handler=.not_implemented_yet,
                  serializer = serializer_unboxed_json())
  services$handle("DELETE",
                  "/<service_id>",
                  handler=.not_implemented_yet,
                  serializer = serializer_unboxed_json())
  services$handle("OPTIONS",
                  "/<service_id>",
                  handler=.cors_option_bypass)
  
  services$filter("authorization",.authorized)
  
  return(services)
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

    config = MapServerConfig$new(obj = raster(files[1]),service=service)
    #TODO maybe we need to create layers for each additional file...
    
    mapfile = paste(openeo.server$workspaces.path,"services",paste(service$service_id,".map",sep=""),sep="/")
    config$toFile(mapfile)
  } else {
    #TODO
  }
  
  return(service$detailedInfo())
}