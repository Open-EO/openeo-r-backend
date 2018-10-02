# data endpoint ----

createDataEndpoint = function() {
  data = plumber$new()
  
  openeo.server$registerEndpoint("/collections/","GET")
  data$handle("GET",
              "/",
              handler = .listData,
              serializer = serializer_unboxed_json())
  data$handle("OPTIONS",
              "/",
              handler = .cors_option_bypass)
  
  openeo.server$registerEndpoint("/collections/{data_id}","GET")
  data$handle("GET",
              "/<pid>",
              handler = .describeData,
              serializer = serializer_unboxed_json())
  data$handle("OPTIONS",
              "/<pid>",
              handler = .cors_option_bypass)
  
  
  # data$handle("GET",
  #             "/opensearch",
  #             handler = .not_implemented_yet,
  #             serializer = serializer_unboxed_json())
  # data$handle("OPTIONS",
  #             "/opensearch",
  #             handler = .cors_option_bypass)
  
  return(data)
}


# creates an overview on products available
#* @get /api/data
#* @serializer unboxedJSON
.listData = function() {
  if (endsWith(openeo.server$baseserver.url,"/")) {
    baseurl = substr(openeo.server$baseserver.url,1,nchar(openeo.server$baseserver.url)-1)
  } else {
    baseurl = openeo.server$baseserver.url
  }
  
  datalist = openeo.server$data
    list(collections=unname(lapply(datalist, function(l){
      return(l$shortInfo())
    })),
    links = list(
      rel="self",
      href=paste(baseurl,"collections",sep="/")
    )
  )
}

# returns details of a certain product
#* @get /api/data/<pid>
#* @serializer unboxedJSON
.describeData = function(req,res,pid) {
  if (pid %in% names(openeo.server$data) == FALSE) {
    return(error(res,404,"Dataset not found"))
  } else {
    return(openeo.server$data[[pid]]$detailedInfo())
  }
}