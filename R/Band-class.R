library(R6)

Band <- R6Class(
  "Band",
  public = list(
    band_id=NULL,
    name=NULL,
    wavelength_nm=NULL,
    res_m=NULL,
    scale=NULL,
    offset=NULL,
    type=NULL,
    unit=NULL,
    
    initialize= function(band_id=NA,name=NA,wavelength_nm=NA,res_m=NA,scale=NA,offset=NA,type=NA,unit=NA) {
      self$band_id=band_id
      self$name=name
      self$wavelength_nm=wavelength_nm
      self$res_m=res_m
      self$scale=scale
      self$offset=offset
      self$type=type
      self$unit=unit
    },
    toList = function() {
      list(
        band_id=self$band_id,
        name=self$name,
        wavelength_nm=self$wavelength_nm,
        res_m=self$res_m,
        scale=self$scale,
        offset=self$offset,
        type=self$type,
        unit=self$unit
      )
    }
    
  )
)