#' @include config.R
#' @include Product-class.R
#' @include Band-class.R
#' @import raster
#' @importFrom GDALinfo rgdal
loadLandsat7Dataset = function() {

  ls7.path = paste(openeo$data.path,"/landsat7/",sep="")
  ls7.filepaths = list.files(path=ls7.path,pattern='*.tif$')
  
  #
  # register empty products
  #
  ls7.product = Product$new("landsat7_ndvi", 
                            "Landsat7 NDVI calculation", 
                            "Marius Appel")
  
  
  #
  # add granules and data to the products
  #
  createGranuleFromLS7NDVIFile = function(filename) {
    time = parseDateFromLS7FileName(filename)
    data = raster(paste(ls7.path,filename,sep=""))
    gr = Granule$new(time=time,data=data,extent=extent(data),srs=crs(data))

    ls7.product$addGranule(granule=gr)
  }
  
  
  parseDateFromLS7FileName = function (filename) {
    doyFormat="%Y%j"
    dayOfYearString = substring(filename,10,16)
    return(strptime(dayOfYearString,format=doyFormat))
  }
  
  lapply(ls7.filepaths, function(file) {
    createGranuleFromLS7NDVIFile(file)
  })
  
  firstGranule = ls7.product$getCollection()$granules[[1]]
  filePath = attr(attr(firstGranule$data,"file"),"name")

  md = GDALinfo(filePath,silent=TRUE)
  scale = attr(md,"ScaleOffset")[,"scale"]
  offset = attr(md,"ScaleOffset")[,"offset"]
  type = tolower(attr(md,"df")[1,"GDType"])
  nodata=attr(md,"df")[1,"NoDataValue"]
  resolution = list(x=md["res.x"],y=md["res.y"])
  
  # add band and finalize information on landsat7 series
  ls7.product$bands = append(ls7.product$bands,
                                           Band$new(band_id=1,
                                                    name="ndvi",
                                                    type = type,
                                                    scale = scale, 
                                                    offset = offset,
                                                    nodata=nodata,
                                                    res=resolution))
  
  
  ls7.product$finalize()
  ls7.product$register()
}

loadData = function() {
  openeo$data = list()
  
  loadLandsat7Dataset()
}


