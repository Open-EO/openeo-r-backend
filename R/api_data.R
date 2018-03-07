############################
#
# data endpoint
#
############################

createDataEndpoint = function() {
  data = plumber$new()
  
  data$handle("GET",
              "/",
              handler = .listData,
              serializer = serializer_unboxed_json())
  data$handle("OPTIONS",
              "/",
              handler = .cors_option_bypass)
  
  data$handle("GET",
              "/<pid>",
              handler = .describeData,
              serializer = serializer_unboxed_json())
  data$handle("OPTIONS",
              "/<pid>",
              handler = .cors_option_bypass)
  
  data$handle("GET",
              "/opensearch",
              handler = .not_implemented_yet,
              serializer = serializer_unboxed_json())
  data$handle("OPTIONS",
              "/opensearch",
              handler = .cors_option_bypass)
  
  return(data)
}


# creates an overview on products available
#* @get /api/data
#* @serializer unboxedJSON
.listData = function(qname,qgeom,qstartdate,qenddate) {
  datalist = openeo.server$data
  unname(lapply(datalist, function(l){
    return(l$shortInfo())
  }))
}

# returns details of a certain product
#* @get /api/data/<pid>
#* @serializer unboxedJSON
.describeData = function(req,res,pid) {
  if (pid %in% names(openeo.server$data) == FALSE) {
    return(error(res,404,"Product not found"))
  } else {
    return(openeo.server$data[[pid]]$detailedInfo())
  }
}