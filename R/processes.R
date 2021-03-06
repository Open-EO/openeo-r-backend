#' @include Server-class.R
#' @include Process-class.R
#' @include dimensionality.R
#' @include parameter_type_definitions.R

# get_collection ====
get_collection = Process$new(
  process_id = "get_collection",
  description = "Loads the EO data into a process",
  args = list(
    Argument$new(
      name = "name",
      description = "the temporal dataset/collection",
      required = TRUE,
      type = "string"
    )
  ),
  summary="Select a collection",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(),
  operation = function(name) {
    logger = Logger$new(process=parent.frame(), job = parent.frame()$job)

    msg = paste("Selecting product:",name)
    if (!is.null(parent.frame()$job)) {
      logger$info(msg)
    } else {
      cat(paste(msg,"\n",sep=""))
    }
    
    
    coll = getCollectionFromImageryStatement(name)
    return(coll)
  }
)

# filter_daterange ====
filter_daterange = Process$new(
  process_id = "filter_daterange",
  description = "filters a data set with a temporal dimension based on a stated start and end date",
  args = list(
    Argument$new(
      name = "imagery",
      description = "the temporal dataset/collection",
      required = TRUE,
      type = "object",
      format = "eodata"
    ),
    Argument$new(
      name = "extent",
      description = "an Array containing start and stop date/timestamp for the query interval",
      required = FALSE,
      type = "temporal-extent"
    )
  ),
  summary="Filter by a date range",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(),
  operation = function(imagery, extent) {
    from=extent[[1]] 
    to=extent[[2]]
    
    logger = Logger$new(process=parent.frame(), job = parent.frame()$job)
    logger$info("Starting filter_daterange")
    #imagery might be an identifier or a function (Process$execute()) or a json process description or a
    # udf or a collection we need to specify that
    
    collection = getCollectionFromImageryStatement(imagery)
    if (nrow(collection$getData()) == 0) logger$error("Trying to perform an operation on a collection with no entries")
    if (is.null(collection)) {
      logger$error("no collection element found in function call")
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
      required = TRUE,
      type = "object",
      format = "eodata"
    ),
    Argument$new(
      name = "bands",
      description = "one or more band ids",
      required = TRUE,
      type = c("string","array"),
      items = "string"
    )
  ),
  summary="Filter by band(s)",
  returns = result.eodata,
  modifier = create_dimensionality_modifier(),
  operation = function(imagery,bands) {
    logger = Logger$new(process=parent.frame(), job = parent.frame()$job)
    
    tryCatch({
      collection = getCollectionFromImageryStatement(imagery)
      if (nrow(collection$getData()) == 0) logger$error("Trying to perform an operation on a collection with no entries")
      logger$info("Filtering for bands")
      return(collection$filterByBands(bands))
    },
    error =function(e){
      logger$error(e$message)
    })
    
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
      required = TRUE,
      type = "object",
      format = "eodata"
    ),
    Argument$new(
      name = "regions",
      description = "The relative link in the user workspace, where to find the geometries file",
      required = TRUE,
      type="string",
      format="url"
    ),
    Argument$new(
      name = "func",
      description = "An aggregation function like 'mean', 'median' or 'sum'",
      required = TRUE,
      type="string"
    )
  ),
  summary="Zonal statistics for polygons on EO data",
  returns=result.vector,
  modifier = create_dimensionality_modifier(remove = list(raster=TRUE),add = list(feature=TRUE)),
  operation = function(imagery,regions,func) {
    logger = Logger$new(process=parent.frame(), job = parent.frame()$job)
    func_name = func
    func = get(tolower(func))
    
    logger$info("Start calculating zonal statistics")
    
    if (startsWith(regions,"/")) {
      regions = gsub("^/","",regions)
    }

    file.path = paste(parent.frame()$user$workspace,"files",regions,sep="/")
    layername = ogrListLayers(file.path)[1]
    logger$info(paste("Found layer:",layername))
    
    regions = readOGR(dsn=file.path,layer = layername)
    logger$info(paste("Opened file",file.path))
    
    polygonList = as.SpatialPolygons.PolygonsList(slot(regions, "polygons"),crs(regions))
    
    # crs(polygonList) = crs(regions)
    logger$info("Imported polygons")
    
    collection = getCollectionFromImageryStatement(imagery)
    
    if (nrow(collection$getData()) == 0) logger$error("Trying to perform an operation on a collection with no entries")
    
    rasterList = unlist(collection$getData() %>% dplyr::select("data"))
    
    b = brick(rasterList)
    values = raster::extract(b,
                             regions,
                             na.rm=TRUE,
                             fun=func,
                             df=FALSE)
    
    logger$info("Finished extracting data for polygons")
    
    old_dimensionality = collection$dimensions
    result = Collection$new(old_dimensionality)
    result$addFeature(space = polygonList,time=(collection$getData() %>% dplyr::select("time")),band=func_name,data=values)
    
    
    return(result)
    
  }
)

# min_time ====
min_time = Process$new(
  process_id = "min_time",
  description = "calculates the minimum value per pixel of a single valued band collection",
  args = list(
    Argument$new(
      name = "imagery",
      description = "the temporal dataset/collection",
      required = TRUE,
      type = "object",
      format = "eodata"
    )
  ),
  summary="Minimum value of collections per pixel",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(remove = list(time=TRUE)),
  operation = function(imagery) {
    logger = Logger$new(process=parent.frame(), job = parent.frame()$job)
    
    logger$info("Starting find_min")
    #get the collection of the imagery
    collection = getCollectionFromImageryStatement(imagery)
    if (nrow(collection$getData()) == 0) logger$error("Trying to perform an operation on a collection with no entries")
    
    if (!collection$dimensions$band) {
      #get a list of the data (raster objects)
      rasters = collection$getData()$data
      
      logger$info("Fetched related granules")
      #create a brick
      data = stack(rasters)
      logger$info("Stacking data")
      
      #calculate
      minimum = calc(data,fun=min,na.rm=T)
      logger$info("calculating the minimum")
      
      #create a granule
      logger$info("creating single granule for minimum calculation")
      
      dims = collection$dimensions
      dims$time = FALSE
      
      #create a collection

      result = Collection$new(dimensions = dims) #modified afterwards
      result$addGranule(data=minimum)
      logger$info("Creating collection for single granule and setting meta data")
      
      return(result)
    } else {
      logger$error("Not implemented yet. Group by band and apply function, or reduce band dimension")
    }
  }
)

# max_time ====
max_time = Process$new(
  process_id = "max_time",
  description = "calculates the maximum value per pixel of a single valued band collection",
  args = list(
    Argument$new(
      name = "imagery",
      description = "the temporal dataset/collection",
      required = TRUE,
      type = "object",
      format = "eodata"
    )
  ),
  summary="Minimum value of collections per pixel",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(remove = list(time=TRUE)),
  operation = function(imagery) {
    logger = Logger$new(process=parent.frame(), job = parent.frame()$job)
    
    logger$info("Starting find_min")
    #get the collection of the imagery
    collection = getCollectionFromImageryStatement(imagery)
    if (nrow(collection$getData()) == 0) logger$error("Trying to perform an operation on a collection with no entries")
    
    if (!collection$dimensions$band) {
      #get a list of the data (raster objects)
      rasters = collection$getData()$data
      
      logger$info("Fetched related granules")
      #create a brick
      data = stack(rasters)
      logger$info("Stacking data")
      
      #calculate
      minimum = calc(data,fun=min,na.rm=T)
      logger$info("calculating the minimum")
      
      #create a granule
      logger$info("creating single granule for minimum calculation")
      
      dims = collection$dimensions
      dims$time = FALSE
      
      #create a collection
      
      result = Collection$new(dimensions = dims) #modified afterwards
      result$addGranule(data=minimum)
      logger$info("Creating collection for single granule and setting meta data")
      
      return(result)
    } else {
      logger$error("Not implemented yet. Group by band and apply function, or reduce band dimension")
    }
  }
)

# filter_bbox ====
filter_bbox = Process$new(
  process_id="filter_bbox",
  description="Subsets an imagery by a specific extent",
  args = list(Argument$new(
                name = "imagery",
                description = "the spatio-temporal dataset/collection",
                required = TRUE,
                type = "object",
                format = "eodata"),
              Argument$new(
                name = "extent",
                description = "Spatial extent as object [west,south,east,north,crs,...], may include a vertical axis (height or depth)",
                required = TRUE,
                type="spatial_extent")
              ),
  summary="Spatial filter with Bounding Box",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(),
  operation = function(imagery, extent) {
    left = extent$west
    right = extent$east
    bottom = extent$south
    top = extent$north
    e = extent(left,right,bottom,top)

    logger = Logger$new(process=parent.frame(), job = parent.frame()$job)
    logger$info("Filter for bounding box")
    collection = getCollectionFromImageryStatement(imagery)
    if (nrow(collection$getData()) == 0) logger$error("Trying to perform an operation on a collection with no entries")
    
    target_crs = collection$getGlobalSRS()
    
    crs = crs("+init=epsg:4326")
    
    if (!is.null(extent$crs)) { # modify crs if it is set
      crs = trim(tolower(extent$crs))
      
      if (!startsWith(crs,"+")) { #if not proj4 string
        if (!startsWith(crs,"epsg:")) crs = paste0("epsg:",crs)
        
        crs = paste0("+init=",crs)
      }
      crs = crs(crs)
      
      
    }
    request_extent = extent2polygon(e,crs)
    
    # transform request extent if request_crs != target_crs
    if (crs@projargs != target_crs@projargs) {
      #transform request into collection crs
      target_geom = spTransform(request_extent,target_crs) # can be distorted, so take the bbox of that again
    } else {
      target_geom = request_extent
    }

    #apply intersection on sf object which also directly filters out outlier
    intersection = st_intersection(collection$space,st_as_sf(target_geom)) 
    
    if (nrow(intersection)==0) logger$error("Selected extent is disjoint to data set.")
    
    space_id_selection = unique(intersection$ID)
    
    collection$space = intersection
    
    # filter
    cropped_data = collection$getData() %>% dplyr::filter(space %in% space_id_selection) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = {
        #TODO think about using a lazy approach
        list(raster::crop(data,as(intersection %>% dplyr::filter(ID==space),"Spatial")))
      }) %>%
      dplyr::ungroup()
    
    output = collection$clone(deep=TRUE)
    output$setData(cropped_data)
    
    return(output)
  }
)

# NDVI ====
NDVI = Process$new(
  process_id = "NDVI",
  description = "Calculates the ndvi per pixel and scene in a given collection",
  args = list(Argument$new(
               name = "imagery",
               description = "the spatio-temporal dataset/collection",
               required = TRUE,
               type = "object",
               format = "eodata"
             ),
             Argument$new(
               name = "nir",
               description = "The band id of the Near Infrared (NIR) band",
               required = TRUE,
               type="string"
             ),
             Argument$new(
               name = "red",
               description = "The band id of the visible red band",
               required = TRUE,
               type="string"
             )),
  summary="NDVI calculation per pixel",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(remove = list(band=TRUE)),
  operation=function(imagery,nir,red) {
    logger = Logger$new(process=parent.frame(), job = parent.frame()$job)
    
    logger$info("Starting calculate_ndvi")

    collection = getCollectionFromImageryStatement(imagery)
    if (nrow(collection$getData()) == 0) logger$error("Trying to perform an operation on a collection with no entries")
    
    nir.index = collection$getBandIndex(nir)
    red.index = collection$getBandIndex(red)
    
    if (is.na(nir.index) || is.na(red.index)) {
      logger$error("Incorrect band indices.")
    }
    
    logger$info("Fetched indices for bands")
    if (collection$dimensions$time) {
      group = collection$getData() %>% group_by(time) 
    } else {
      group = collection$getData()
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

    logger$info("ndvi calculation applied on all granules")
    
    result.collection = collection$clone(deep=TRUE)
 
    result.collection$setData(ndvi_calculation)
    logger$info("set metadata for newly calculated collection")
    
    return(result.collection)
  }
)

# aggregate_time ====
aggregate_time = Process$new(
  process_id = "aggregate_time",
  description = "Applies UDF of type aggregate_time on an object of class Collection",
  args = list(Argument$new(
                name = "imagery",
                description = "the spatio-temporal dataset/collection",
                required = TRUE,
                type = "object",
                format = "eodata"
                ),
              Argument$new(
                name = "script",
                description = "the URL or path relative to the current working directory to the user's R script containing the UDF definition",
                required = TRUE,
                type="string",
                format="url"
                ),
              Argument$new(
                name="udf_runtime",
                description = "the identifier which UDF strategy is used.",
                type="string"
              )
  ),
  summary="UDF: Applies an aggregation function over time.",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(remove = list(time = TRUE)),
  operation = function(imagery, script,udf_runtime=NULL) {
    parent = parent.frame()
    job = parent$job
    user = parent$user
    
    logger = Logger$new(process=parent, job = parent.frame()$job)
    
    collection = getCollectionFromImageryStatement(imagery)
    if (nrow(collection$getData()) == 0) logger$error("Trying to perform an operation on a collection with no entries")
    
    if (startsWith(script, "/"))
    {
      script = gsub("^/", "", script)
    }
    
    if (!is.null(udf_runtime) && udf_runtime %in% names(openeo.server$udf_runtimes)) {
      udf_runtime = openeo.server$udf_runtimes[[udf_runtime]]
    } else {
      # pick the default
      udf_runtime = openeo.server$udf_runtimes[[1]]
    }
    
    # fla: if the file is hosted at this backend
    # else we need to download it first.
    
    # prepare paths
    udf_transaction = prepare_udf_transaction(user,script,job$job_id)
    
    oldwd = getwd()
    
    tryCatch({
      return(udf_runtime$performTransaction(collection = collection,
                                            udf_transaction=udf_transaction,
                                            importDimensionality="11110",
                                            dimensionalityModifier = parent.frame()$dimensions_modifier))
    }, 
    error = function(e) {
      logger$error(paste("ERROR:",e))
      udf_transaction = udf_transaction$load()
      udf_transaction$status = "error"
      udf_transaction$end_date = NA
      udf_transaction$store()
    },finally = {
      udf_transaction$clearExportData()
      
      setwd(oldwd)
      
    })
    
    
    
  }
)

# apply_pixel ====
apply_pixel = Process$new(
  process_id = "apply_pixel",
  description = "Applies UDF of type apply_pixel on an object of class Collection",
  args = list(Argument$new(
    name = "imagery",
    description = "the spatio-temporal dataset/collection",
    required = TRUE,
    type = "object",
    format = "eodata"
  ),
  Argument$new(
    name = "script",
    description = "the URL or path relative to the current working directory to the user's R script containing the UDF definition",
    required = TRUE,
    type="string",
    format="url"
  ),
  Argument$new(
    name="udf_runtime",
    description = "the identifier which UDF strategy is used.",
    type="string"
  )
  ),
  summary="UDF: applies a function pixel wise",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(remove = list(band = TRUE)),
  operation = function(imagery, script,udf_runtime=NULL) {
    parent = parent.frame()
    job = parent$job
    user = parent$user
    
    logger = Logger$new(process=parent, job = job)
    
    collection = getCollectionFromImageryStatement(imagery)
    if (nrow(collection$getData()) == 0) logger$error("Trying to perform an operation on a collection with no entries")
    
    if (startsWith(script, "/"))
    {
      script = gsub("^/", "", script)
    }
    
    if (!is.null(udf_runtime) && udf_runtime %in% names(openeo.server$udf_runtimes)) {
      udf_runtime = openeo.server$udf_runtimes[[udf_runtime]]
    } else {
      # pick the default
      udf_runtime = openeo.server$udf_runtimes[[1]]
    }
    
    # fla: if the file is hosted at this backend
    # else we need to download it first.
    # prepare paths
    udf_transaction = prepare_udf_transaction(user,script,job$job_id)
    
    oldwd = getwd()
    
    
    tryCatch({
      return(udf_runtime$performTransaction(collection = collection, 
                                            udf_transaction=udf_transaction,
                                            importDimensionality = "11110",
                                            dimensionalityModifier = parent.frame()$dimensions_modifier))
    }, 
    error = function(e) {
      logger$error(paste("ERROR:",e))
      udf_transaction = udf_transaction$load()
      udf_transaction$status = "error"
      udf_transaction$end_date = NA
      udf_transaction$store()
    },finally= {
      udf_transaction$clearExportData()
      
      setwd(oldwd)
    })
    
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
    if (imagery %in% names(openeo.server$data)) {
      collection = openeo.server$data[[imagery]]$getCollection()
    } else {
      stop(paste("Cannot find product:",imagery))
    }
    
    
  } else if (is.ExecutableProcess(imagery)) {
    collection = imagery$execute()
  } 
  # since accessing the collection is a process we don't any other type check here
  
  if (is.null(collection)) {
    stop("no collection element found in function call")
  }
  return (collection)
}

