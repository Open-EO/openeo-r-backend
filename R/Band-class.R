#' @importFrom R6 R6Class
#' @export
Band <- R6Class(
  "Band",
  public = list(
    band_id=NULL,
    name=NULL,
    wavelength_nm=NULL,
    res=NULL,
    scale=NULL,
    offset=NULL,
    type=NULL,
    unit=NULL,
    nodata=NULL,
    
    initialize= function(band_id=NA,name=NA,wavelength_nm=NA,res=NA,scale=NA,offset=NA,type=NA,unit=NA,nodata=NA) {
      self$band_id=band_id
      self$name=name
      self$wavelength_nm=wavelength_nm
      self$res=res
      self$scale=scale
      self$offset=offset
      self$type=type
      self$unit=unit
      self$nodata=nodata
    },
    toList = function() {
      list(
        band_id=self$band_id,
        name=self$name,
        wavelength_nm=self$wavelength_nm,
        res=self$res,
        scale=self$scale,
        offset=self$offset,
        type=self$type,
        unit=self$unit,
        nodata=self$nodata
      )
    }
    
  )
)