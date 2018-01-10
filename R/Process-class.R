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
#' @field operation A function that executes the operation in R described by this process
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @export
Process <- R6Class(
  "Process",
  public = list(
    process_id = NULL,
    args = NULL,
    description = NULL,
    operation = NULL,
    
    initialize = function(process_id = NA,
                          description = NA,
                          args = NA,
                          operation = NA) {
      self$process_id = process_id
      self$description = description
      self$args = args
      self$operation = operation
    },
    
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
    
    as.executable = function(json, job) {
      if (is.null(job)) {
        stop("No job defined for this executable process")
      }
      
      #return a process where the arguments from the parsed json file are set for
      #this "args". E.g. set a value for args[["from"]]$value and set Process$executable to TRUE
      
      # json at this point is the named list of the process graph provided by the json stored under jobs
      args = json$args
      
      runner = self$clone(deep=TRUE)
      
      clonedArguments = list()
      #deep copy also the arguments
      for (arg in self$args) {
        clonedArguments=append(clonedArguments,arg$clone(deep=TRUE))
      }
      runner$args = clonedArguments
      
      for (key in 1:length(args)) {
        value = args[[key]]
        
        #TODO maybe add a handling for UDF or in the UDF class 
        if (class(value) == "list" && "process_id" %in% names(value)) {
          runner$args[[key]]$value= job$loadProcess(value)
        } else {
          runner$args[[key]]$value = value
        }
      }
      
      return(ExecutableProcess$new(process=runner))
    }
    
  )
)

isProcess = function(obj) {
  return("Process" %in% class(obj))
}