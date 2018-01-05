#' @include config.R
#' @include Process-class.R

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
  operation = function(imagery, from=NA, to=NA) {
    #imagery might be an identifier or a function (Process$execute()) or a json process description or a
    # udf or a collection we need to specify that
    collection = NULL
    
    if ("Product" %in% class(imagery)) {
      collection = imagery$getCollection()
    } else if ("Collection" %in% class(imagery)) {
      collection = imagery
    } else if (class(imagery) == "character") {
      #load image or create process
    } else if ("Process" %in% class(imagery)) {
      collection = imagery$execute()
    } else if (class(imagery) == "list") {
      if ("product_id" %in% names(imagery)) {
        collection = openeo$data[[imagery$product_id]]$getCollection()
      }
    }
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
    collection$filterByTime(from=from, to=to)
  }
)

find_min = Process$new(
  process_id = "find_min",
  description = "calculates the minimum value of a single valued band collection",
  args = list(
    Argument$new(
      name = "imagery",
      description = "the temporal dataset/collection",
      required = TRUE
    )
  ),
  operation = function(imagery) {
    #get the collection of the imagery
    collection = getCollectionFromImageryStatement(imagery)
    
    #get a list of the data (raster objects)
    rasters = lapply(collection$granules, function(obj){obj$data})
    
    #create a brick
    data = stack(rasters)
    
    #calculate
    minimum = calc(data,fun=min,na.rm=T)
    
    #create a granule
    aggregation = Granule$new(time=collection$getMinTime(),data=minimum,extent=extent(minimum),srs=crs(minimum))
    
    #create a collection
    collection = Collection$new()
    collection$addGranule(aggregation)
    collection$sortGranulesByTime
    
    return(collection)
  }
)

registerProcesses = function() {
  openeo$processes = list()
  
  
  filter_daterange$register()
  
  
  #filter_sp_extent = Process$new()
  #filter_sp_extent$register()
  
  #crop_extent = Process$new()
  #crop_extent$register()
  
  
  
  find_min$register()
  
  #calculate_ndvi = Process$new()
  #calculate_ndvi$register()
  
  
}

#' Resolves imagery statement
#' 
#' This function resolves the imagery statement defined in a process. It can be
#' either a product or an intermediate calculation. In any case the result shall
#' be a collection on which the calculations shall be performed.
getCollectionFromImageryStatement = function (imagery) {
  collection = NULL
  
  if ("Product" %in% class(imagery)) {
    collection = imagery$getCollection()
  } else if ("Collection" %in% class(imagery)) {
    collection = imagery
  } else if (class(imagery) == "character") {
    #load image or create process
  } else if ("Process" %in% class(imagery)) {
    collection = imagery$execute()
  } else if (class(imagery) == "list") {
    if ("product_id" %in% names(imagery)) {
      collection = openeo$data[[imagery$product_id]]$getCollection()
    }
  }
  if (is.null(collection)) {
    stop("no collection element found in function call")
  }
  return (collection)
}

