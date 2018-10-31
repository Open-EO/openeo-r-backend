############################
#
# processes endpoint
#
############################

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