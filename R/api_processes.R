############################
#
# processes endpoint
#
############################

createProcessesEndpoint = function() {
  process = plumber$new()
  
  openeo.server$registerEndpoint("/processes/","GET")
  process$handle("GET",
                 "/",
                 handler = .listProcesses,
                 serializer = serializer_unboxed_json())
  process$handle("OPTIONS",
                 "/",
                 handler = .cors_option_bypass)
  
  # process$handle("GET",
  #                "/<pid>",
  #                handler = .describeProcess,
  #                serializer = serializer_unboxed_json())
  # process$handle("OPTIONS",
  #                "/<pid>",
  #                handler = .cors_option_bypass)
  # 
  # process$handle("GET",
  #                "/opensearch",
  #                handler = .not_implemented_yet,
  #                serializer = serializer_unboxed_json())
  # process$handle("OPTIONS",
  #                "/opensearch",
  #                handler = .cors_option_bypass)
  
  return(process)
}


# creates an overview on available processes
#* @get /api/processes
#* @serializer unboxedJSON
.listProcesses = function(qname) {
  processeslist = openeo.server$processes
  list(processes=unname(lapply(processeslist, function(l){
    return(l$detailedInfo())
  })),
  links=list())
}

# returns details of a certain product
#* @get /api/processes/<pid>
#* @serializer unboxedJSON
.describeProcess = function(req,res,pid) {
  if (pid %in% names(openeo.server$processes) == FALSE) {
    return(error(res,404,"Process not found"))
  } else {
    return(openeo.server$processes[[pid]]$detailedInfo())
  }
}