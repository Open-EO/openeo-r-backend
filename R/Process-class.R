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
#' @field executable A flag whether or not this process is a template or is executable
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
    executable=FALSE,
    
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
    
    register = function () {
      name = self$process_id
      process = self
      
      addable = list(process)
      
      names(addable) = c(name)
      
      openeo$processes = append(openeo$processes, addable)
    },
    
    execute = function() {
      if (self$executable) {
        #build a list of parameter (key=value) for operation
        parameter = list()
        for (key in names(self$args)) {
          value = self$args[[key]]$value
          
          if (class(value)[1] == "Process") {
            parameter[[key]] = value$execute()
          } else {
            parameter[[key]] = value
          }
          
          
        }
        
        return(do.call(self$operation,parameter))
      } else {
        stop("This template process cannot be executed.")
      }
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
      runner$executable = TRUE
      
      for (key in names(args)) {
        value = args[[key]]
        
        #TODO maybe add a handling for UDF or in the UDF class 
        if (class(value) == "list" && "process_id" %in% names(value)) {
          runner$args[[key]]$value= job$loadProcess(value)
        } else {
          runner$args[[key]]$value = value
        }
      }
      
      return(runner)
    }
    
  )
)

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

#' Constructor
#' 
#' The constructor for this Argument class
#' 
#' @name Argument$new
#' 