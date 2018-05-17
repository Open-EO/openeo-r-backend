#' @include Server-class.R
#' @include Product-class.R
#' @include Band-class.R
#' @import raster
#' @importFrom rgdal GDALinfo
loadLandsat7Dataset = function() {

  data.path = openeo.server$data.path
  
  if (endsWith(data.path,"/")) {
    ls7.path = paste(data.path,"landsat7/",sep="")
  } else {
    ls7.path = paste(data.path,"/landsat7/",sep="")
  }
  
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
    filePath = paste(ls7.path,filename,sep="")
    data = raster(filePath)
    gr = Granule$new(time=time,data=data,extent=extent(data),srs=crs(data))
    
    # add band information
    md = GDALinfo(filePath,silent=TRUE)
    scale = attr(md,"ScaleOffset")[,"scale"]
    offset = attr(md,"ScaleOffset")[,"offset"]
    type = tolower(attr(md,"df")[1,"GDType"])
    nodata=attr(md,"df")[1,"NoDataValue"]
    resolution = list(x=md["res.x"],y=md["res.y"])
    
    gr$addBands(Band$new(band_id=1,
                         name="ndvi",
                         type = type,
                         scale = scale, 
                         offset = offset,
                         nodata=nodata,
                         res=resolution))

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
  
  ls7.product$setDimensionality(create_dimensionality(space=TRUE,time=TRUE,band=TRUE,raster=TRUE))
  
  ls7.product$deriveMetadata()
  openeo.server$register(ls7.product)
}

loadSentinel2Data = function() {
  sentinel2.folder = openeo.server$data.path
  if (endsWith(sentinel2.folder,"/")) {
    sentinel2.folder = paste(sentinel2.folder,"sentinel2",sep="")
  } else {
    sentinel2.folder = paste(sentinel2.folder,"sentinel2",sep="/")
  }
  
  loadSentinel2Scene = function(folder.path, vrt.filename) {
    image.paths = list.files(folder.path, pattern="*.tif", full.names = TRUE)
    vrt.paths = list.files(folder.path, pattern="*.vrt$",full.names = TRUE)
    
    vrt.path = NULL
    if (length(vrt.paths) == 0) {
      #create a new vrt, otherwise use vrt to load!
      separator = NULL
      if (endsWith(folder.path,"/")) {
        separator = ""
      } else {
        separator = "/"
      }
      
      vrt.path = paste(folder.path,vrt.filename,sep=separator)
      
      gdalbuildvrt(gdalfile=image.paths,output.vrt=vrt.path,resolution="highest",separate = TRUE)
    } else {
      vrt.path = vrt.paths[1]
    }
    
    return(stack(vrt.path))
  }
  
  parseDateFromSentinel2FileName = function (filename) {
    if (length(filename) > 1) {
      filename = filename[1]
    }
    timestampFormat ="%Y%m%dT%H%M%S"
    timestamp = substring(filename,12,26)
    return(strptime(timestamp,format=timestampFormat))
  }
  
  createSentinelBandInformation = function(file.path, band_id, name=NA, wavelength=NA) {
    
    md = GDALinfo(file.path,silent=TRUE)
    scale = attr(md,"ScaleOffset")[,"scale"]
    offset = attr(md,"ScaleOffset")[,"offset"]
    type = tolower(attr(md,"df")[1,"GDType"])
    nodata=attr(md,"df")[1,"NoDataValue"]
    resolution = list(x=md["res.x"],y=md["res.y"])
    
    return(Band$new(band_id=band_id,
                    name=name,
                    wavelength_nm = wavelength,
                    type = type,
                    scale = scale, 
                    offset = offset,
                    nodata=nodata,
                    res=resolution))
  }
  
  # create empty product with some information
  sentinel2.product = Product$new("sentinel2_subset", 
                                  "Sentinel 2 raster time series for a small spatial subset", 
                                  "ESA / Marius Appel")
  
  sentinel2.foldernames = list.files(sentinel2.folder)
  sentinel2.folderpaths = list.files(sentinel2.folder,full.names = TRUE)
  
  # iterate over all sub folders in the sentinel2 data folder and create granules for a collection
  for (i in 1 : length(sentinel2.foldernames)) {
    folder = sentinel2.foldernames[i]
    path = sentinel2.folderpaths[i]
    
    image.filenames = list.files(path,pattern="*.tif$")
    image.filepaths = list.files(path,pattern="*.tif$",full.names = TRUE)
    
    # extract date time from file name
    datetime = parseDateFromSentinel2FileName(image.filenames)
    
    # create raster stack
    stack = loadSentinel2Scene(path, paste(folder,".vrt",sep=""))
    
    # create granule
    granule = Granule$new(data=stack,time=datetime)
    
    #Add Bands
    ids = c("1","2","3","4","5","6","7","8","8a","9","10","11","12")
    names = c(NA,"blue","green","red",NA,NA,NA,"nir",NA,NA,NA,NA,NA)
    wavelengths = c("443","490","560","665","705","740","783","842","865","940","1375","1610","2190")
    
    band.list = list()
    
    for (i in 1:13) {
      band.list = append(band.list,list(createSentinelBandInformation(file.path = image.filepaths[i],
                                                                      band_id=ids[i],
                                                                      name = names[i],
                                                                      wavelength = wavelengths[i])))
    }
    granule$addBands(band.list)
    
    # add granule to product
    sentinel2.product$addGranule(granule)
  }
  
  sentinel2.product$setDimensionality(create_dimensionality(space=TRUE,time=TRUE,band=TRUE,raster=TRUE))
  
  sentinel2.product$deriveMetadata()
  openeo.server$register(sentinel2.product)
  
}


