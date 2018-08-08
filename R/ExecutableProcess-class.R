#' @importFrom R6 R6Class
#' @include Process-class.R
#' @export
ExecutableProcess <- R6Class(
  "ExecutableProcess",
  inherit = Process,
  # public ----
  public = list(
    # attributes ====
    job = NULL,
    user = NULL,
    # functions ====
    initialize= function(process_id = NA,
                        description = NA,
                        args = NA,
                        operation = NA,
                        process= NULL) {
      
      if (!is.null(process)) {
        variables = names(process)
        for (key in variables) {
          value = process[[key]]
          if (class(value) == "function" || class(value) == "environment" || key == "min_parameters") { # min_parameters is active..
            next()
          } else {
            self[[key]] = value
          }
        }
        self$operation = process$operation
      } else {
        self$process_id = process_id
        self$description = description
        self$args = args
        self$operation = operation
      }
    },
    
    execute = function() {
        #build a list of parameter (key=value) for operation
        parameter = list()
        for (key in 1:length(self$args)) {
          name = self$args[[key]]$name
          value = self$args[[key]]$value
      
          if (is.ExecutableProcess(value)) {
            parameter[[name]] = value$execute()
          } else {
            parameter[[name]] = value
          }

        }
        result = do.call(self$operation,parameter,envir = self)
        # modify dimensionality
        result$dimensions = dim.apply(result$dimensions, self$dimensions_modifier)
        
        return(result)
    },
    
    detailedInfo = function() {
      args = list()
      
      args=lapply(self$args, function(argument) {
        # this needs to be the unlisted version since valueInfo is also used to get a simplified represenation of processes
        argument$valueInfo()
      })
      return(list(
        process_id = self$process_id,
        args = args
      ))
    }
  )
)

# statics ----
is.ExecutableProcess = function(obj) {
  return(all(c("ExecutableProcess", "Process") %in% class(obj)) )
}