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
    })
)
