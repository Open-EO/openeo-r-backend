#' Write generics to disk
#' 
#' @description This function writes rasters in a consistent directory structure to disk in generic GeoTIFF formats. 
#' It takes as input an object of class `Collection` and the path where the files are to be written to disk. Once the files 
#' have been written to disk, it can be loaded into a `stars` object by the user after which custom functions could 
#' be applied to it.
#'
#' @param collection_obj Object of class Collection as produced in the previous step while executing the process graph before encountering the UDF
#' @param dir_name Path where the generics are to be written to disk. This could be obtained from the UDF process if it is defined by the user while registering it.
#'
#' @export
#'
write_generics = function(collection_obj, dir_name = "disk") #dir_name could be obtained if it is defined while registering the UDF
{
  scene_table = collection_obj$getData()
  R2G_obj = R2Generic$new(scenes = scene_table)
  R2G_obj$write_scenes(dir_name = dir_name)
  
  R2G_obj$legend_to_disk(dir_name)

}

#' Prepares the collection data for the UDF service request
#' 
#' Transforms the data contained in a Collection into a JSON representation. It will be passed along the code script URL as data
#' to the specified UDF REST processing service. Currently implemented only for raster timeserires collections.
#' 
#' @param collection Collection object
#' @param strategy the tiling strategy (not implemented yet)
#' @return list that can be transformed into "UdfData" JSON
#' @export
udf_export = function(collection,strategy) {
  if (! is.Collection(collection)) {
    stop("Passed object is not a Collection object")
  }
  
  # TODO prepare some sort of tiling strategy
  
  if (collection$dimensions$raster && collection$dimensions$space && collection$dimensions$time) {
    udf_data = list()
    udf_data[["proj"]] = as.character(collection$getGlobalSRS())
    udf_data[["raster_collection_tiles"]] = list()
    
    udf_data[["raster_collection_tiles"]] = append(udf_data[["raster_collection_tiles"]],raster_collection_export(collection))
    return(udf_data)
  } else {
    stop("Not yet implemented")
  }
  
}

udf_request = function(collection,strategy=NULL,udf_transaction) {
  # TODO remove the hard coded backend selection
  request = list(
    code = list(
      language = "R",
      source = readChar(udf_transaction$script, file.info(udf_transaction$script)$size)
    ),
    data = udf_export(collection = collection, strategy = strategy)
  )
  
  return(request)
}

#' Creates RasterCollectionTile representation
#' 
#' Subsets and groups Collection data by band and space in order to create the specified UDF RasterCollectionTile JSON output.
#' 
#' @param collection Collection object
#' @return list that can be transformed into "UdfData" JSON
#' @export
raster_collection_export = function(collection) {
  if (! is.Collection(collection)) {
    stop("Passed object is not a Collection object")
  }
  
  data = collection$getData()
  extents = collection$space
  
  modified = data %>% group_by(band,space) %>% dplyr::summarise(
    exported = tibble(band,space,data,time) %>% (function(x,...) {
      raster_collection_tiles = list()
      raster_collection_tiles[["id"]] = "test1"
      
      raster_collection_tiles[["wavelength"]] = unique(x[["band"]])
      
      # select sf polygons by ids stored in the data table, then give bbox from all of the sf
      b = st_bbox(extents[x %>% dplyr::select(space) %>% unique() %>% unlist() %>% unname(),])
      raster_collection_tiles[["extent"]] = list(
        north = b[["ymax"]],
        south = b[["ymin"]],
        west = b[["xmin"]],
        east = b[["xmax"]],
        height = yres(x[[1,"data"]]),
        width = xres(x[[1,"data"]])
      )
      
      # times
      times = x[["time"]]
      tres = round(median(diff(times)))
      raster_collection_tiles[["start_times"]] = strftime(times, "%Y-%m-%dT%H:%M:%S", usetz = TRUE)
      raster_collection_tiles[["end_times"]] = strftime(times+tres, "%Y-%m-%dT%H:%M:%S", usetz = TRUE)
      
      # fetch data from raster files as matrix (store as list first other wise it messes up the matrix 
      # structure by creating row/col as rows for each attribute)
      raster_collection_tiles[["data"]] = x %>% apply(MARGIN=1,FUN= function(row) {
        
        return(list(raster::values(x=row$data, format="matrix")))
        
      })
      
      # unlist it again
      raster_collection_tiles[["data"]] = lapply(raster_collection_tiles[["data"]], function(arr_list) {
        arr_list[[1]]
      })
      
      return(list(raster_collection_tiles))
    })
  )
  return(modified[["exported"]])
}

prepare_udf_transaction = function(user,script,job_id = NULL) {
  udf = Udf$new()
  
  # TODO mayb script is URL
  isURL = FALSE
  
  # TODO implement the URL check and also download script if necessary
  if (isURL) {
    # download the script and store it in the user workspace
    script.url = script
    file.path = script.url
  } else {
    # then we need to make the script accessable as URL
    file.path = paste(user$workspace,"files", script, sep="/")
  }
  
  udf$script = file.path
  
  if (!is.null(job_id)) {
    udf$job_id = job_id
  } else {
    udf$job_id = "sync_job"
  }
  
  udf$store()

  return(udf)
}