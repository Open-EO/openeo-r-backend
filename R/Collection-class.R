#' @include Granule-class.R
#' @importFrom R6 R6Class
#' @export
Collection <- R6Class(
  "Collection",
  public = list(
    granules = NULL,
    initialize = function() {
      self$granules = list()
    },
    addGranule = function(granule) {
      self$granules = append(self$granules,granule)
    },
    getMinTime = function() {
      self$granules[[1]]$time
    },
    getMaxTime = function() {
      self$granules[[length(self$granules)]]$time
    },
    sortGranulesByTime= function () {
      self$granules = self$granules[
        order(
          sapply(
            self$granules,
            function(g){
              as.POSIXct(g$time)
            }
          )
        )]
    },
    calculateExtent = function() {
      globalExtent = self$granules[[1]]$extent
      
      lapply(self$granules, function(g) {
        globalExtent <<- union(globalExtent,g$extent)
      })
      
      return(globalExtent)
    },
    getGlobalSRS = function() {
      if(length(self$granules) >= 1) {
        return(self$granules[[1]]$srs)
      } else {
        return(NULL)
      }
      
    }
  )
  
)