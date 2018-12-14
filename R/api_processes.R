#
# processes endpoint ----
#

# creates an overview on available processes
#* @get /api/processes
#* @serializer unboxedJSON
.listProcesses = function(qname) {
  tryCatch({
    return(
      list(processes=unname(lapply(openeo.server$processes, function(process){
        return(process$detailedInfo())
      })),
      links=list())
    )
  }, error = handleError)
  
}