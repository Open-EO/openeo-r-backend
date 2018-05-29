#' @include Server-class.R
#' @include Process-class.R
#' @include dimensionality.R

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
  modifier = create_dimensionality_modifier(),
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
  modifier = create_dimensionality_modifier(),
  operation = function(imagery,bands) {
    collection = NULL
    
    collection = getCollectionFromImageryStatement(imagery)
    cat("Filtering for bands")
    return(collection$filterByBands(bands))
  }
)

# zonal_statistic ====
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
  modifier = create_dimensionality_modifier(remove = list(raster=TRUE),add = list(feature=TRUE)),
  operation = function(imagery,regions,func) {
    func = get(tolower(func))
    
    if (startsWith(regions,"/")) {
      regions = gsub("^/","",regions)
    }
    
    file.path = paste(openeo.server$workspaces.path,regions,sep="/")
    layername = ogrListLayers(file.path)[1]
    
    regions = readOGR(dsn=file.path,layer = layername)
    
    polygonList = as.SpatialPolygons.PolygonsList(slot(regions,layername))
    
    collection = getCollectionFromImageryStatement(imagery)
    rasterList = as.list(collection$getData() %>% select("data"))
    
    timestamps = (collection$getData() %>% select("time"))[[1]]

    b = brick(rasterList)
    
    values = raster::extract(b,
                             regions,
                             na.rm=TRUE,
                             fun=func,
                             df=TRUE)
    
    colnames(values) = c(colnames(regions@data),timestamps)
    out = SpatialPolygonsDataFrame(polygonList,data=values)
    crs(out) <- crs(regions)
    
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
  modifier = create_dimensionality_modifier(remove = list(time=TRUE)),
  operation = function(imagery) {
    cat("Starting find_min\n")
    #get the collection of the imagery
    collection = getCollectionFromImageryStatement(imagery)
    
    #get a list of the data (raster objects)
    rasters = as.list(collection$getData() %>% select("data"))
    
    cat("Fetched related granules\n")
    #create a brick
    data = stack(rasters)
    cat("Stacking data\n")
    
    #calculate
    minimum = calc(data,fun=min,na.rm=T)
    cat("calculating the minimum\n")
    
    #create a granule
    aggregation = Granule$new(time=collection$getMinTime(),data=minimum,extent=extent(minimum),srs=crs(minimum))
    cat("creating single granule for minimum calculation\n")
    
    dims = collection$dimensions
    
    #create a collection
    collection = Collection$new(dimensions = dims) #modified afterwards
    collection$addGranule(aggregation)
    collection$sortGranulesByTime
    cat("Creating collection for single granule and setting meta data\n")
    
    return(collection)
  }
)

# filter_bbox ====
filter_bbox = Process$new(
  process_id="filter_bbox",
  description="Subsets an imagery by a specific extent",
  args = list(Argument$new(name = "imagery",
                           description = "the spatio-temporal dataset/collection",
                           required = TRUE),
              Argument$new(name = "left",
                           description = "The left value of a spatial extent",
                           required = TRUE),
              Argument$new(name = "right",
                           description = "The right value of a spatial extent",
                           required = TRUE),
              Argument$new(name = "bottom",
                           description = "The bottom value of a spatial extent",
                           required = TRUE),
              Argument$new(name = "top",
                           description = "The top value of a spatial extent",
                           required = TRUE)),
  modifier = create_dimensionality_modifier(),
  operation = function(imagery, left, right, bottom, top) {
    collection = getCollectionFromImageryStatement(imagery)
    e = extent(left,right,bottom,top)
    
    srs = collection$getGlobalSRS()
    
    data_table = collection$getData()
    
    #TODO filter for intersection first!!! also calculate intersections
    cropped_data = data_table %>% dplyr::mutate(data = list(raster::crop(data[[1]],y=e)),
                                                space = list(extent2polygon(e,srs)))
    
    output = collection$clone(deep=TRUE)
    output$setData(cropped_data)
    
    return(output)
  }
)

# calculate_ndvi ====
calculate_ndvi = Process$new(
  process_id = "calculate_ndvi",
  description = "Calculates the ndvi per pixel and scene in a given collection",
  args = list(Argument$new(
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
  modifier = create_dimensionality_modifier(remove = list(band=TRUE)),
  operation=function(imagery,nir,red) {
    cat("Starting calculate_ndvi\n")
    
    collection = getCollectionFromImageryStatement(imagery)
    nir.index = collection$getBandIndex(nir)
    red.index = collection$getBandIndex(red)
    cat("Fetched indices for bands\n")
    
    if (self$dimensions$time) {
      group = private$data_table %>% group_by(time) 
    } else {
      group = private$data_table
    }
    
    ndvi_calculation = group %>% 
      filter(band %in% c(red.index,nir.index)) %>% 
      arrange(band) %>% 
      dplyr::summarise(space = list(first(space)), data= tibble(data) %>% (function(x,...){
        s = stack(x$data)
        ndvi = calc(s, fun= function(x) {
          (x[2] - x[1])/(x[2] + x[1])
        })
        return(list(ndvi))
      }))
    
    # fetch the data elements and simultanously calculate ndvi

    cat("ndvi calculation applied on all granules\n")
    
    result.collection = Collection$new()
    result.collection$dimensions = collection$dimensions # will be updated in $execute()
 
    result.collection$setDate(ndvi_calculation)
    cat("set metadata for newly calculated collection\n")
    
    return(result.collection)
  }
)

# Resolves imagery statement
# 
# This function resolves the imagery statement defined in a process. It can be
# either a product or an intermediate calculation. In any case the result shall
# be a collection on which the calculations shall be performed.
#' @export
getCollectionFromImageryStatement = function (imagery) {
  collection = NULL
  if (is.Product(imagery)) {
    collection = imagery$getCollection()
  } else if (is.Collection(imagery)) {
    collection = imagery
  } else if (class(imagery) == "character") {
    #load image or create process
  } else if (is.ExecutableProcess(imagery)) {
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

