# 
# need to use source inclusion at the api level, since plumber
# loads this script and creates the api. any functions / variables
# or else defined at package level is not visible here elsewise.
# 
#' @include Granule-class.R
#' @include Collection-class.R
#' @include Product-class.R
#' @include config.R
#' @include data.R
source("R/config.R")
source("R/Granule-class.R")
source("R/Collection-class.R")
source("R/Product-class.R")
source("R/data.R")


#* @get /api/version
#* @serializer unboxedJSON
function() {
  list(version=openeo$api.version)
}

# creates an overview on products available
#* @get /api/data
#* @serializer unboxedJSON
function() {
  datalist = openeo$data
  unname(lapply(datalist, function(l){
    return(l$shortInfo())
  }))
}

# returns details of a certain product
#* @get /api/data/<pid>
#* @serializer unboxedJSON
function(req,res,pid) {
  if (pid %in% names(openeo$data) == FALSE) {
    return(error(res,404,"Product not found"))
  } else {
    return(openeo$data[[pid]]$detailedInfo())
  }
}

error = function(res, status,msg) {
  res$status = status
  
  return(list(
    status=status,
    message=msg)
    )
}
