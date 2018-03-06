#' @include Server-class.R
#' @include Process-class.R

# filter_daterange ====
filter_daterange = Process$new(
  process_id = "filter_daterange",
  description = "filters a data set with a temporal dimension based on a stated start and end date",
  args = list(
    Argument$new(
      name = "imagery",
      description = "the temporal dataset/collection",
      required = TRUE
    ),
    Argument$new(
      name = "from",
      description = "start date/timestamp for the query interval",
      required = FALSE
    ),
    Argument$new(
      name = "to",
      description = "end date/timestamp for the query interval",
      required = FALSE
    )
  ),
  operation = function(imagery, from=NULL, to=NULL) {
    cat("Starting filter_daterange\n")
    #imagery might be an identifier or a function (Process$execute()) or a json process description or a
    # udf or a collection we need to specify that
    collection = NULL
    
    collection = getCollectionFromImageryStatement(imagery)
    if (is.null(collection)) {
      stop("no collection element found in function call")
    }
    
    if (!is.null(from) && class(from) == "character") {
      from = as.POSIXct(from)
    }
    
    if (!is.null(to) && class(to) == "character") {
      to = as.POSIXct(to)
    }
    
    # collection is at this point a Collection
    return(collection$filterByTime(from=from, to=to))
  }
)

# filter_bands ====
filter_bands = Process$new(
  process_id = "filter_bands",
  description = "filters by single and multiple band ids",
  args = list(
    Argument$new(
      name = "imagery",
      description = "the temporal dataset/collection",
      required = TRUE
    ),
    Argument$new(
      name = "bands",
      description = "one or more band ids",
      required = TRUE
    )
  ),
  operation = function(imagery,bands) {
    collection = NULL
    
    collection = getCollectionFromImageryStatement(imagery)
    cat("Filtering for bands")
    return(collection$filterByBands(bands))
  }
)

zonal_statistics = Process$new(
  process_id = "zonal_statistics",
  description = "Calculates the zonal statistics from a given file containing polygons and returns a spatial polygon dataframe. It should not be nested in other calls which require imagery.",
  args = list(
    Argument$new(
      name = "imagery",
      description = "The imagery data set on which the zonal statistics shall be performed",
      required = TRUE
    ),
    Argument$new(
      name = "regions",
      description = "The relative link in the user workspace, where to find the geometries file",
      required = TRUE
    ),
    Argument$new(
      name = "func",
      description = "An aggregation function like 'mean', 'median' or 'sum'",
      required = TRUE
    )
  ),
  operation = function(imagery,regions,func) {
    func = get(tolower(func))
    
    file.path = paste(openeo.server$workspaces.path,regions,sep="/")
    layername = ogrListLayers(file.path)[1]
    
    regions = readOGR(dsn=file.path,layer = layername)
    
    polygonList = as.SpatialPolygons.PolygonsList(slot(regions,layername))
    
    collection = getCollectionFromImageryStatement(imagery)
    rasterList = lapply(collection$granules, function(granule) {
      return(granule$data)
    })
    timestamps = sapply(collection$granules, function(granule) {
      return(as.character(granule$time))
    })
    b = brick(rasterList)
    
    values = raster::extract(b,
                             regions,
                             na.rm=TRUE,
                             fun=func,
                             df=TRUE)
    
    colnames(values) = c(colnames(data@data),timestamps)
    out = SpatialPolygonsDataFrame(polygonList,data=values)
    
    return(out)
    
  }
)

# find_min ====
find_min = Process$new(
  process_id = "find_min",
  description = "calculates the minimum value per pixel of a single valued band collection",
  args = list(
    Argument$new(
      name = "imagery",
      description = "the temporal dataset/collection",
      required = TRUE
    )
  ),
  operation = function(imagery) {
    cat("Starting find_min\n")
    #get the collection of the imagery
    collection = getCollectionFromImageryStatement(imagery)
    
    #get a list of the data (raster objects)
    rasters = lapply(collection$granules, function(obj){obj$data})
    cat("Fetched related granules\n")
    #create a brick
    data = stack(rasters)
    cat("Stacking data")
    
    #calculate
    minimum = calc(data,fun=min,na.rm=T)
    cat("calculating the minimum\n")
    
    #create a granule
    aggregation = Granule$new(time=collection$getMinTime(),data=minimum,extent=extent(minimum),srs=crs(minimum))
    cat("creating single granule for minimum calculation\n")
    
    #create a collection
    collection = Collection$new()
    collection$addGranule(aggregation)
    collection$sortGranulesByTime
    cat("Creating collection for single granule and setting meta data\n")
    
    return(collection)
  }
)

# calculate_ndvi ====
calculate_ndvi = Process$new(
  process_id = "calculate_ndvi",
  description = "Calculates the ndvi per pixel and scene in a given collection",
  arg = list(Argument$new(
               name = "imagery",
               description = "the spatio-temporal dataset/collection",
               required = TRUE
             ),
             Argument$new(
               name = "nir",
               description = "The band id of the Near Infrared (NIR) band",
               required = TRUE
             ),
             Argument$new(
               name = "red",
               description = "The band id of the visible red band",
               required = TRUE
             )),
  operation=function(imagery,nir,red) {
    cat("Starting calculate_ndvi\n")
    collection = getCollectionFromImageryStatement(imagery)
    nir.index = collection$getBandIndex(nir)
    red.index = collection$getBandIndex(red)
    cat("Fetched indices for bands\n")
    
    # fetch the data elements and simultanously calculate ndvi
    rasters = lapply(collection$granules, function(obj){
      data = obj$data
      ndvi = calc(data, fun= function(x) {
        (x[nir.index] - x[red.index])/(x[nir.index] + x[red.index])
      })
      
      granule = Granule$new(time = obj$time,
                            data = ndvi,
                            bands = list(
                              ndvi = Band$new(
                                band_id = "ndvi"
                              )
                            ))
      return(granule)
    })
    cat("ndvi calculation applied on all granules\n")
    
    result.collection = Collection$new()
    result.collection$granules = rasters
    result.collection$sortGranulesByTime()
    cat("set metadata for newly calculated collection\n")
    
    return(result.collection)
  }
)

# Resolves imagery statement
# 
# This function resolves the imagery statement defined in a process. It can be
# either a product or an intermediate calculation. In any case the result shall
# be a collection on which the calculations shall be performed.
getCollectionFromImageryStatement = function (imagery) {
  collection = NULL
  if (isProduct(imagery)) {
    collection = imagery$getCollection()
  } else if (isCollection(imagery)) {
    collection = imagery
  } else if (class(imagery) == "character") {
    #load image or create process
  } else if (isExecutableProcess(imagery)) {
    collection = imagery$execute()
  } else if (class(imagery) == "list") {
    if ("product_id" %in% names(imagery)) {
      collection = openeo.server$data[[imagery$product_id]]$getCollection()
    }
  }
  if (is.null(collection)) {
    stop("no collection element found in function call")
  }
  return (collection)
}

