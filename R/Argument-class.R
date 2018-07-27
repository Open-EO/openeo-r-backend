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
    # attributes ====
    name = NULL,
    description = NULL,
    
    type = NULL,
    format = NULL,
    items = NULL,
    examples = NULL,
    
    required = NULL,
    value = NULL,
    
    # public ====
    initialize = function(name = NA,
                          description = NA,
                          type = NA,
                          format = NA,
                          items = NA,
                          examples = NA,
                          required = FALSE) {
      self$name = name
      self$description = description
      self$type = type
      self$format = format
      self$items = items
      self$examples = examples
      self$required = required
    },
    
    shortInfo = function() {
      res = list()
      
      res[[self$name]] = list(description = self$description)
      return(res)
    },
    
    detailedInfo = function() {
      res = list()
      res[[self$name]] = .generic_param(description = self$description,
                                        required = self$required,
                                        type = self$type,
                                        format = self$format,
                                        items = self$items,
                                        examples = self$examples)
        
      return(res)
    },
    
    valueInfo = function() {
      arg = list()
      value = NULL
      
      if (is.ExecutableProcess(self$value)) {
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

# static =====
is.Argument = function(obj) {
  return("Argument" %in% class(obj))
}
