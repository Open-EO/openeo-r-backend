#' Error codes and messages
#' 
#' This data set contains the error codes and messages that shall be used in the openEO back-ends
#' 
#' @docType data
#' @usage data(errors)
#' 
#' @format a tibble
"errors"



handleError = function(e) {
  error_obj = fromJSON(e$message)
  
  # hack the system... go up the parent.frames until we find "res" R6 class for plumber response
  i = 1
  env=parent.frame(n = i)
  while (!"res" %in% names(env)) {
    i = i+1
    env=parent.frame(n = i)
  }
  
  env$res$status = error_obj$status
  
  # id and links are spared for now
  return(list(
    code = error_obj$code,
    message=error_obj$msg,
    links = list())
  )
}

#test 
# tryCatch(stop("{\"code\":2102,\"msg\":\"Process 'NDVI' does not support argument 'banane'.\",\"status\":400}",call. = FALSE),
#          error=handleError)

throwError = function(id, ...) {
  # errors exists at runtime. it will be called by data("errors")
  
  # errors$code - openeo specific error code
  # errors$name - human readable label for the error
  # errors$description - human readable description of the error
  # errors$msg - the predefined error message with variables
  # errors$status - the HTTP status code to be returned by this error
  # errors$parameter - list of variable names to be replaced by information in runtime
  
  result = errors %>% filter(name == id)
  
  if (nrow(result) == 1) {
    result = as.list(result)
    result$description = NULL
    result$name = NULL
    result$parameter = unlist(result$parameter)
    # resolve ...
    variables = list(...)
    
    if (length(result$parameter) > 0) {
      # replace each variable
      for (index in seq_along(result$parameter)) {
        variable_name = result$parameter[[index]]
        
        if (! variable_name %in% names(variables)) {
          # replace with empty
          value = "<missing variable>"
        } else {
          value = variables[[variable_name]]
        }
        result$msg = gsub(x=result$msg, pattern=paste0("\\{",variable_name,"\\}"),replacement = paste0(value))
      }
    }
    
    
    result$parameter = NULL
    stop(toJSON(result,auto_unbox = TRUE))
  } else {
    stop(toJSON(list(code=NULL,msg=NULL,status=500),auto_unbox = TRUE))
  }
}
