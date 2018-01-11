#' Argument
#' 
#' This class represent the arguments of an openeo backend process. It serves two purposes:
#' First it is used to describe the functions argument for the "describe" webservice calls, but
#' it is also able to store the values that are passed to the backend for execution
#' 
#' @field name The name of the argument
#' @field description A short description of what the argument is about
#' @field required A flag whether or not this argument is required for process execution or not
#' @field value The actual value that is used for processing
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
Argument <- R6Class(
  "Argument",
  public = list(
    name = NULL,
    description = NULL,
    required = NULL,
    value = NULL,
    
    initialize = function(name = NA,
                          description = NA,
                          required = FALSE) {
      self$name = name
      self$description = description
      self$required = required
    },
    
    shortInfo = function() {
      res = list()
      
      res[[self$name]] = list(description = self$description)
      return(res)
    },
    
    valueInfo = function() {
      arg = list()
      value = NULL
      
      if (isExecutableProcess(self$value)) {
        value = self$value$detailedInfo()
      } else if (class(self$value) == "list" && names(self$value)[1] == "product_id") {
        value = list(product_id = self$value$product_id)
      } else {
        value = self$value
      }
      
      arg[[self$name]] = value
      
      return(arg)
    }
  )
)

isArgument = function(obj) {
  return("Argument" %in% class(obj))
}
