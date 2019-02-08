# data endpoint ----


# creates an overview on products available
#* @get /api/data
#* @serializer unboxedJSON
.listData = function() {
  tryCatch({
    baseurl = openeo.server$configuration$baseserver.url
    
    datalist = openeo.server$data
      list(collections=unname(lapply(datalist, function(l){
        return(l$shortInfo())
      })),
      links = list(
        rel="self",
        href=paste(baseurl,"collections",sep="/")
      )
    )
  }, error=handleError)
}

# returns details of a certain product
#* @get /api/data/<pid>
#* @serializer unboxedJSON
.describeData = function(req,res,name) {
  tryCatch({
    if (name %in% names(openeo.server$data) == FALSE) {
      throwError("CollectionNotFound")
    } else {
      return(openeo.server$data[[name]]$detailedInfo())
    }
  },
  error=handleError)
  
}