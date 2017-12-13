#' @importFrom R6 R6Class
#' @export
Process <- R6Class(
  public = list(
    process_id = NULL,
    args = NULL,
    description=NULL,
    execute = NULL,
    
    initialize = function(process_id=NA,description=NA,args=NA,execute=NA) {
      self$process_id = process_id
      self$description = description
      self$args = args
      self$execute = execute
    },
    
    shortInfo = function() {
      list(
        process_id = self$process_id,
        description = self$description
      )
    },
    detailedInfo = function() {
      
      arglist=list()
      for(arg in self$args) {
        arglist = append(arglist,arg$shortInfo())
      }
      
      res = list(
        process_id = self$process_id,
        description = self$description,
        args = arglist
      )
      
      return(res)
    }
    
  )  
)

#' @importFrom R6 R6Class
#' @export
Argument <- R6Class(
  public = list(
    name=NULL,
    description=NULL,
    required=NULL,
    value=NULL,
    
    initialize = function(name=NA,description=NA,required=FALSE) {
      self$name = name
      self$description = description
      self$required = required
    },
    
    shortInfo = function() {
      res = list()
      
      res[[self$name]] = list(
        description=self$description
      )
      return(res)
    }
  )
)