# data endpoint ----


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
.describeData = function(req,res,name) {
  if (name %in% names(openeo.server$data) == FALSE) {
    return(error(res,404,"Dataset not found"))
  } else {
    return(openeo.server$data[[name]]$detailedInfo())
  }
}