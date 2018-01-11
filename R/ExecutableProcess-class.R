#' @importFrom R6 R6Class
#' @include Process-class.R
#' @export
ExecutableProcess <- R6Class(
  "ExecutableProcess",
  inherit = Process,
  public = list(
    executable=FALSE,
    
    initialize= function(process_id = NA,
                        description = NA,
                        args = NA,
                        operation = NA,
                        process= NULL) {
      
      if (!is.null(process)) {
        variables = names(process)
        
        for (key in variables) {
          value = process[[key]]
          if (class(value) == "function" || class(value) == "environment") {
            next()
          } else {
            self[[key]] = value
          }
        }
        self$operation <- process$operation
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
      
          if (isExecutableProcess(value)) {
            parameter[[name]] = value$execute()
          } else {
            parameter[[name]] = value
          }

        }
        
        return(do.call(self$operation,parameter))
    },
    
    detailedInfo = function() {
      args = list()
      
      args=lapply(self$args, function(argument) {argument$valueInfo()})
      
      return(list(
        process_id = self$process_id,
        args = args
      ))
    }
  )
)

isExecutableProcess = function(obj) {
  return(all(c("ExecutableProcess", "Process") %in% class(obj)) )
}