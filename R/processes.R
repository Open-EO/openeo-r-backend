#' @include Server-class.R
#' @include Process-class.R
#' @include dimensionality.R
#' @include parameter_type_definitions.R

# get_data ====
get_data = Process$new(
  process_id = "get_data",
  description = "Loads the EO data into a process",
  args = list(
    Argument$new(
      name = "data_id",
      description = "the temporal dataset/collection",
      required = FALSE,
      type = "string"
    )
  ),
  summary="Filter by a date range",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(),
  operation = function(data_id) {
    logger = Logger$new(process=self, job = parent.frame()$job)
    
    msg = paste("Selecting product:",data_id)
    if (!is.null(parent.frame()$job)) {
      logger$info(msg)
    } else {
      cat(paste(msg,"\n",sep=""))
    }
    
    
    coll = getCollectionFromImageryStatement(data_id)
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
      name = "from",
      description = "start date/timestamp for the query interval",
      required = FALSE,
      type = "string",
      format = "date-time"
    ),
    Argument$new(
      name = "to",
      description = "end date/timestamp for the query interval",
      required = FALSE,
      type = "string",
      format = "date-time"
    )
  ),
  summary="Filter by a date range",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(),
  operation = function(imagery, from=NULL, to=NULL) {
    logger = Logger$new(process=self, job = parent.frame()$job)
    logger$info("Starting filter_daterange")
    #imagery might be an identifier or a function (Process$execute()) or a json process description or a
    # udf or a collection we need to specify that
    collection = NULL
    
    collection = getCollectionFromImageryStatement(imagery)
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
    logger = Logger$new(process=self, job = parent.frame()$job)
    collection = NULL
    
    collection = getCollectionFromImageryStatement(imagery)
    logger$info("Filtering for bands")
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
    logger = Logger$new(process=self, job = parent.frame()$job)
    func_name = func
    func = get(tolower(func))
    
    logger$info("Start calculating zonal statistics")
    
    if (startsWith(regions,"/")) {
      regions = gsub("^/","",regions)
    }
    
    file.path = paste(openeo.server$workspaces.path,regions,sep="/")
    layername = ogrListLayers(file.path)[1]
    
    regions = readOGR(dsn=file.path,layer = layername)
    
    polygonList = as.SpatialPolygons.PolygonsList(slot(regions,layername))
    crs(polygonList) = crs(regions)
    logger$info("Imported polygons")
    
    collection = getCollectionFromImageryStatement(imagery)
    
    rasterList = unlist(collection$getData() %>% dplyr::select("data"))
    
    timestamps = unlist(collection$getData() %>% dplyr::select("time") %>% dplyr::transmute(as.character(time)))

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

# find_min ====
find_min = Process$new(
  process_id = "find_min",
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
    logger = Logger$new(process=self, job = parent.frame()$job)
    
    logger$info("Starting find_min")
    #get the collection of the imagery
    collection = getCollectionFromImageryStatement(imagery)
    
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
                name = "left",
                description = "The left value of a spatial extent",
                required = TRUE,
                type="number"),
              Argument$new(
                name = "right",
                description = "The right value of a spatial extent",
                required = TRUE,
                type="number"),
              Argument$new(
                name = "bottom",
                description = "The bottom value of a spatial extent",
                required = TRUE,
                type="number"),
              Argument$new(
                name = "top",
                description = "The top value of a spatial extent",
                required = TRUE,
                type="number")
              ),
  summary="Spatial filter with Bounding Box",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(),
  operation = function(imagery, left, right, bottom, top) {
    logger = Logger$new(process=self, job = parent.frame()$job)
    
    logger$info("Filter for bounding box")
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
    logger = Logger$new(process=self, job = parent.frame()$job)
    
    logger$info("Starting calculate_ndvi")

    collection = getCollectionFromImageryStatement(imagery)
    nir.index = collection$getBandIndex(nir)
    red.index = collection$getBandIndex(red)
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
                )
  ),
  summary="UDF: Applies an aggregation function over time.",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(remove = list(time = TRUE)),
  operation = function(imagery, script) {
    parent = parent.frame()
    job = parent$job
    user = parent$user
    
    logger = Logger$new(process=self, job = parent.frame()$job)
    
    collection = getCollectionFromImageryStatement(imagery)
    if (startsWith(script, "/"))
    {
      script = gsub("^/", "", script)
    }
    
    
    # fla: if the file is hosted at this backend
    # else we need to download it first.
    
    # prepare paths
    udf_transaction = prepare_udf_transaction(user,script,job$job_id)
    
    udf_transaction$prepareExportData(collection,export_type=c("json","file"))
    
    # # export data
    # write_generics(collection,dir_name = udf_transaction$workspace)
    # #testing
    # write(toJSON(udf_request(collection=collection,udf_transaction = udf_transaction),auto_unbox=TRUE,pretty = TRUE),paste(udf_transaction$workspace,"udf_request.json",sep="/"))
    # 
    oldwd = getwd()
    
    tryCatch({
      setwd(udf_transaction$workspace) 
      source(file = udf_transaction$script, local = TRUE) 
      # Now read back results present at results.file.path
      # To be implemented once classes for data I/O have been re-written
      # The argument "code" will eventually be evaulated from the dimensions of "collection" and "modifier" 
      # -> modification is applied afterwards
      
      # TODO replace code with something that is read from a global meta data file
      result.collection = read_legend(legend.path = paste(udf_transaction$results_workspace, "out_legend.csv", sep = "/"), code = "11110")
      
      udf_transaction = udf_transaction$load()
      udf_transaction$status = "finished"
      udf_transaction$end_date = format(now(),format="%Y-%m-%d %H:%M:%S")
      udf_transaction$store()
      
      return(result.collection)
    }, 
    error = function(e) {
      logger.error(paste("ERROR:",e))
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
  )
  ),
  summary="UDF: applies a function pixel wise",
  returns=result.eodata,
  modifier = create_dimensionality_modifier(remove = list(band = TRUE)),
  operation = function(imagery, script) {
    parent = parent.frame()
    job = parent$job
    user = parent$user
    
    logger = Logger$new(process=self, job = job)
    
    collection = getCollectionFromImageryStatement(imagery)
    if (startsWith(script, "/"))
    {
      script = gsub("^/", "", script)
    }
    
    # fla: if the file is hosted at this backend
    # else we need to download it first.
    # prepare paths
    udf_transaction = prepare_udf_transaction(user,script,job$job_id)
    
    udf_transaction$prepareExportData(collection,export_type="file")
    
    oldwd = getwd()
    
    tryCatch({
      setwd(udf_transaction$workspace) 
      
      source(file = udf_transaction$script, local = TRUE) 

      # TODO replace code with something that is read from a global meta data file
      result.collection = read_legend(legend.path = paste(udf_transaction$results_workspace, "out_legend.csv", sep = "/"), code = "11110")
      
      udf_transaction = udf_transaction$load()
      udf_transaction$status = "finished"
      udf_transaction$end_date = format(now(),format="%Y-%m-%d %H:%M:%S")
      udf_transaction$store()
      
      return(result.collection)
    }, 
    error = function(e) {
      logger$error(paste("ERROR:",e))
      udf_transaction = udf_transaction$load()
      udf_transaction$status = "error"
      udf_transaction$end_date = NA
      udf_transaction$store()
    },finally= function() {
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
  } else if (class(imagery) == "list") {
    if ("data_id" %in% names(imagery)) {
      collection = openeo.server$data[[imagery$product_id]]$getCollection()
    }
  }
  if (is.null(collection)) {
    stop("no collection element found in function call")
  }
  return (collection)
}

