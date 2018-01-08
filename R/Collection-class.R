#' Collection
#' 
#' This class represents the collections, which contain a set of Granules. Collections are also used to transfer
#' intermediate results from one process to an other (the result of a process is a Collection).
#' 
#' @field granules A list of Granules that shall be sorted by time ascending
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
    filterByTime = function(from=NA,to=NA) {
      if (is.na(from)) {
        from = self$getMinTime()
      }
      if (is.na(to)) {
        to = self$getMaxTime()
      }
      
      indices = .collection.filterbytime(self$granules,from,to)
      
      res = self$clone(deep=TRUE)
      res$granules = res$granules[indices$min : indices$max]
      return(res)
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
      
    },
    getBandNames = function() {
      firstGranule = self$granules[[1]]
      return(names(firstGranule$bands))
    },
    getBandIndex = function(band_id) {
      return(which(band_id == self$getBandNames()))
    }
  )
  
)

.collection.filterbytime = function (granules,from,to) {
  minpos = -1
  maxpos = -1
  
  if (from > to) {
    old <- to
    to <- from
    from <- old
  }
  
  if (length(granules) == 0) return(NULL)
  
  for (i in 1:length(granules)) {
    currentGranule = granules[[i]]
    if (!is.null(currentGranule)) {
      if (currentGranule$time >= from && minpos < 0) {
        minpos = i
      }
      
      if (currentGranule$time > to && maxpos < 0) {
        maxpos = i-1
      }
    }
    if (minpos >= 0 && maxpos >= 0) {
      break
    }
  }
  return(list(min = minpos, max = maxpos))
}

isCollection = function(obj) {
  return("Collection" %in% class(obj))
}