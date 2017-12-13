#' @importFrom R6 R6Class
#' @export
Granule <- R6Class(
  "Granule",
  public=list(
    time=NULL,
    extent=NULL,
    srs=NULL,
    data=NULL,
    bands=NULL,
    
    initialize = function(time=NA,extent=NA,srs=NA,data=NA,bands=NA) {
      self$time=time
      self$extent=extent
      self$srs=srs
      self$data=data
      self$bands=bands
    }
  )
)
