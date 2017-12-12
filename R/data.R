#' @include config.R
#' @include Product-class.R
#' @include Band-class.R

library(raster)


  ls7.filepaths = list.files(path="data/landsat7/",pattern='*.tif$')
  
  #
  # register empty products
  #
  openeo$data <- list(landsat7_ndvi = Product$new("landsat7_ndvi", 
                                                  "Landsat7 NDVI calculation", 
                                                  "Marius Appel")
                      )
  
  #
  # add granules and data to the products
  #
  createGranuleFromLS7NDVIFile = function(filename) {
    time = parseDateFromLS7FileName(filename)
    data = raster(paste("data/landsat7/",filename,sep=""))
    gr = Granule$new(time=time,data=data,extent=extent(data),srs=crs(data))

    openeo$data$landsat7_ndvi$addGranule(granule=gr)
  }
  
  
  
  parseDateFromLS7FileName = function (filename) {
    doyFormat="%Y%j"
    dayOfYearString = substring(filename,10,16)
    return(strptime(dayOfYearString,format=doyFormat))
  }
  
  lapply(ls7.filepaths, function(file) {
    createGranuleFromLS7NDVIFile(file)
  })
  openeo$data$landsat7_ndvi$bands = append(openeo$data$landsat7_ndvi$bands,
                                           Band$new(band_id=1,name="ndvi"))
  openeo$data$landsat7_ndvi$finalize()

