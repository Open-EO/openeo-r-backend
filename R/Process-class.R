#' Process
#' 
#' The process class in the openEO thinking. It is a described function that can be serialized in short and detail for the webservice, 
#' but also relates to the internal R function (process) that will be applied to perform the task.
#' 
#' @details The class is used to describe a processing entity, which means it stores its description (textual), the described
#' Arguments (see Argument) and the R operation. The class is mainly used as a template class, but it can be made executable. 
#' When the process is executable then the arguments will have values assigned to them.
#' 
#' @field process_id The id or name of the process which will be used to be addressed from the webservice
#' @field args The list of arguments for the underlying function
#' @field description The textual description of the process
#' @field dimensions_modifier A DimensionModifier class that will be applied to the input data to describe how the data is changed during
#' processing.
#' @field operation A function that executes the operation in R described by this process
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @include dimensionality.R
Process <- R6Class(
  "Process",
  # public ----
  public = list(
    # attributes ====
    process_id = NULL,
    args = NULL,
    description = NULL,
    dimensions_modifier = NULL,
    
    # functions ====
    initialize = function(process_id = NA,
                          description = NA,
                          args = NA,
                          operation = NULL,
                          modifier=NULL) {
      self$process_id = process_id
      self$description = description
      self$args = args
      self$operation = operation
      if (is.null(modifier) || class(modifier) != "DimensionalityModifier") {
        self$dimensions_modifier = create_dimensionality_modifier()
      } else {
        self$dimensions_modifier = modifier
      }
      
    },
    
    operation = NULL, # will be assigned with a function during the initialization
    
    shortInfo = function() {
      list(process_id = self$process_id,
           description = self$description)
    },
    detailedInfo = function() {
      arglist = list()
      for (arg in self$args) {
        arglist = append(arglist, arg$shortInfo())
      }
      res = list(
        process_id = self$process_id,
        description = self$description,
        args = arglist
      )
      
      return(res)
    },
    
    
    setArgumentValue = function(name, value) {
      # arguments are unnamed so list(Argument) -> list(Argument:name)
      argument_names = lapply(self$args,function(argument) {
        return(argument$name)
      })
      
      # get index of argument
      x = match(name,argument_names)
      
      if (is.na(x)) {
        stop(paste("Cannot find argument: '",name,"' in process '",self$process_id,"'",sep=""))
      }
      
      # set value for argument at index "x"
      self$args[[x]]$value = value
      
      invisible(self)
    }
    
  )
)

# statics ====

#' @export
is.Process = function(obj) {
  return("Process" %in% class(obj))
}