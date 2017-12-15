#' @importFrom R6 R6Class
#' @export
Process <- R6Class(
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
          parameter[[key]] = value
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

#' @importFrom R6 R6Class
#' @export
Argument <- R6Class(
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