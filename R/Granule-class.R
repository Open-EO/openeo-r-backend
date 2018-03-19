#' Granule
#' 
#' Smalles unit of an image collection in openEO sense. The granule is the snapshot of a part of the Earth at a given 
#' point in time.
#' 
#' @field time The POSIXt object when the image was created
#' @field extent The spatial extent
#' @field srs Spatial Reference System as defined by the crs() function in the raster package
#' @field data Raster* object which holds the data
#' @field bands Named List of Band objects
#' 
#' @include Band-class.R
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
    
    initialize = function(time=NULL,extent=NULL,srs=NULL,data=NULL,bands=NULL) {
      self$time=time
      self$extent=extent
      self$srs=srs
      self$data=data
      
      if (! is.null(data)) {
        if (class(data)[1] %in% c("RasterBrick", "RasterLayer", "RasterStack")) {
          if (is.null(extent)) {
            self$extent = extent(data)
          }
          
          if (is.null(srs)) {
            self$srs = crs(data)
          }
        }
      }
      
      self$bands=bands
    },
    addBands = function(bands) {
      if (is.list(bands)) {
        if (is.null(names(bands))) {
          # create a named list
          names = sapply(bands, function(band) {return(paste(band$band_id,sep=""))})
          names(bands) <- names
        }
      } else if (class(bands)[1] == "Band") {
        band_id = bands$band_id
        bands = list(bands)
        names(bands) <- c(band_id)
      }
      
      self$bands = append(self$bands,bands)
    },
  
    getBandIndices = function(bandids) {
      ids = names(self$bands)
      
      selector = sapply(bandids,function(val) {
        return(paste(val))
      })
      return(match(selector, ids))
    })
)

isGranule = function(obj) {
  return("Granule" %in% class(obj))
}
