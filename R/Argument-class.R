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
    }
  )
)

isArgument = function(obj) {
  return("Argument" %in% class(obj))
}
