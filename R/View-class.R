#' @docType class
#' @importFrom R6 R6Class
#' @export
View <- R6Class(
  "View",
  public = list(
    space = NULL,
    time = NULL,
    
    initialize = function() {
      self$space = SpaceView$new()
      self$time = TimeView$new()
    }
  )
)

#' @export
SpaceView <- R6Class(
  "SpaceView",
  public = list(
    srs = NULL,
    window = list(
      left = NULL,
      right = NULL,
      top = NULL,
      bottom = NULL
    ),
    cell_size = NULL,
    resampling = NULL,
    
    initialize = function() {
      
    }
  )
)

#' @export
TimeView <- R6Class(
  "TimeView",
  public = list(
    window = list(
      start = NULL,
      end = NULL
    ),
    timestep = NULL,
    resampling = NULL,
    
    initialize = function() {
      
    }
  )
)