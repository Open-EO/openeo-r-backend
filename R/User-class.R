#' @docType class
#' @importFrom R6 R6Class
#' @export
User <- R6Class(
  "User",
  public = list(
    user_id = NULL,
    workspace = NULL,
    files = NULL,
    jobs = NULL,
    user_name = NULL,
    password = NULL,
    token = NULL,
    
    initialize = function(user_id) {
      self$user_id = user_id
    },
    
    toList = function() {
      return(
        list(
          user_id = self$user_id,
          user_name = self$user_name,
          password = self$password,
          jobs = self$jobs
        )
      )
    }
  )
)

isUser = function(obj) {
  return(all("User" %in% class(obj)))
}