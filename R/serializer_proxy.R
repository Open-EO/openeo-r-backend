#' Proxy serializer
#' 
#' This serializer will serialize the data of a httr response. The body will become content(val) and the
#' content-type is set accordingly. Also the status code will be transferred from httr-response to the httpuv response.
#' 
#' @return a serializer function that passes through the response - as is - from the redirected server
#' @export
serializer_proxy <- function(){
  function(val, req, res, errorHandler){
    tryCatch({
      #val is httr response
      res$setHeader("Content-Type",val$headers[["content-type"]])
      
      res$body <- content(val,as="raw")
      res$status = val$status_code
      
      return(res$toResponse())
    }, error=function(e){
      errorHandler(req, res, e)
    })
  }
}