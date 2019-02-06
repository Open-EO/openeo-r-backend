#' A constructor for UdfRuntime class
#' 
#' A S3 class to store all relevant information about a udf runtime environment for visualization purposes to a user (used 
#' at /udf_runtimes endpoint)
#' 
#' @param id the id at which the udf runtime will be referencable at openeo.server$udf_runtimes
#' @param description a textual description about the udf runtime environment
#' @param language the underlying programming language to execute the udf script in
#' @param version the version of the programming langunge
#' @param libraries a list of installed libraries of the programming language
#' @param docker the docker image name (either on dockerhub or on a private repository)
#' @param tag the tag of the docker image
#' @param links a link to point to additional information
#' @param performTransaction a function that handles the interaction between back-end and UDF runtime
#' 
#' @details The performTransaction function is very important! It handles how the UDF is externalized, run and reimported. The UDF processes
#' need to take care of building a UDF transaction object which will work as the link to the calling job or service. The 'performTransaction'
#' then needs to state how to interact with the UDF runtime (hence perform transaction). The function is required to look like this:
#' 
#' function(collection, udf_transaction, importDimensionality,dimensionalityModifier) { ... }
#' 
#' collection = the collection that comes into the transaction
#' udf_transaction = the UdfTransaction object
#' importDimensionality = (optional) Dimensionality that is used when importing a collection
#' dimensionalityModifier = DimensionalityModifier that is applied on collection to describe the change
#' 
#' The importDimensionality is currently a legacy artifact from the R2Generic implementation.
#' 
#' @return UdfRuntime class
UdfRuntime = function(id, 
                      description=NULL, 
                      language, 
                      version = NULL, 
                      libraries = NULL, 
                      docker = NULL, 
                      tag=NULL, 
                      links = NULL, 
                      performTransaction) {
  class_obj = list(
    id = id,
    language = language,
    performTransaction = performTransaction
  )
  
  if (!is.null(description)) class_obj[["description"]] = description
  
  if (!is.null(version)) class_obj[["version"]] = version
  
  if (!is.null(libraries)) class_obj[["libraries"]] = libraries
  
  if (!is.null(docker)) class_obj[["docker"]] = docker
  
  if (!is.null(tag)) class_obj[["tag"]] = tag
  
  if (!is.null(links)) class_obj[["links"]] = links
  
  class(class_obj) = "UdfRuntime"
  
  return(class_obj)
}


is.UdfRuntime = function(x) {
  return(!is.null(class(x)) && class(x) == "UdfRuntime")
}

#' UDF runtime filebased
#' 
#' Creates a UDF runtime that is able to run the UDF in the R local filebased approach.
r_filebased_udf_runtime = function() {
  
  libraries = as_data_frame(installed.packages()) %>%
    dplyr::select(Package,Version) %>% 
    rowwise() %>%
    do(Package = .$Package, 
       obj = list(version=unname(.$Version),
                        links =
                          list(
                          href = paste0("https://CRAN.R-project.org/package=",.$Package)
                          ))
      )
    
  libs = libraries$obj
  names(libs) = libraries$Package
  
  return(UdfRuntime(
    id = paste0("R-",paste0(R.version$major,".",R.version$minor),"-cmd"),
    language = "R",
    version = paste0(R.version$major,".",R.version$minor),
    description = "Filebased commandline approach in R.",
    libraries = libs,
    performTransaction = function(collection, 
                                  udf_transaction, 
                                  importDimensionality,
                                  dimensionalityModifier) {
      
      dimensionalityModifier = dimensionalityModifier$remove_dimension
      udf_transaction$prepareExportData(collection,export_type="file", dim_mod = names(which(dimensionalityModifier == TRUE)))
      
      setwd(udf_transaction$workspace) 
      
      source(file = udf_transaction$script, local = TRUE) 
      
      # Now read back results present at results.file.path
      # To be implemented once classes for data I/O have been re-written
      # The argument "code" will eventually be evaulated from the dimensions of "collection" and "modifier"
      # -> modification is applied afterwards
      
      # TODO replace code with something that is read from a global meta data file
      result.collection = read_legend(legend.path = paste(udf_transaction$results_workspace, "out_legend.csv", sep = "/"), 
                                      code = importDimensionality)
      
      udf_transaction = udf_transaction$load()
      udf_transaction$status = "finished"
      udf_transaction$end_date = format(now(),format="%Y-%m-%d %H:%M:%S")
      udf_transaction$store()
      
      return(result.collection)
    }
  ))
}

#' Create the service runtime
#' 
#' Creates a UDFRuntime object for the server in order to run an UDF on the R-UDF webservice.
r_udf_service_runtime = function() {
  
  # just leave it for now...
  libs = list()
  
  return(UdfRuntime(
    id = paste0("R-3.5.1-udf-service"),
    language = "R",
    version = "3.5.1",
    description = "Approach using the R UDF webservice with a array-based JSON exchange.",
    libraries = libs,
    performTransaction = function(collection, 
                                  udf_transaction, 
                                  importDimensionality,
                                  dimensionalityModifier) {

      dims = dim.apply(collection$dimensions,dimensionalityModifier)
      result.collection = udf_json_to_collection(data = httr::content(httr::POST(url=openeo.server$configuration$rudfservice.url,
                                                                                 body= udf_transaction$prepareExportData(collection,
                                                                                                                         export_type="json", 
                                                                                                                         dim_mod = names(which(dimensionalityModifier$remove_dimension == TRUE))),
                                                                                 encode = c("json"))), 
                                                 dimensions=dims)
      
      
      udf_transaction = udf_transaction$load()
      udf_transaction$status = "finished"
      udf_transaction$end_date = format(now(),format="%Y-%m-%d %H:%M:%S")
      udf_transaction$store()
      
      return(result.collection)
    }
  ))
}

tile_to_raster = function(tile, crs=NULL) {
  length_y = length(tile$data[[1]])
  length_x = length(tile$data[[1]][[1]])
  
  image = raster(matrix(unlist(tile$data[[1]]),nrow=length_y,ncol=length_x)) # should be transposed
  image = flip(image, direction = 1)
  
  extent(image) = c(tile$extent$west,
                    tile$extent$east,
                    tile$extent$south,
                    tile$extent$north)
  
  if (!is.null(crs)) {
    crs(image) = crs
  }
  
  return(image)
}

udf_json_to_collection = function(data, collection=NULL, dimensions=NULL) {
  if (is.null(collection) && is.null(dimensions)) stop("Cannot build collection, please provide either collection to 
                                                       add tiles to or state the dimensionality of the collection")
  
  srs = crs(data$proj)
  
  if (is.null(collection)) {
    collection = Collection$new(dimensions)
  }
  
  for (tile_index in 1:length(data$raster_collection_tiles)){
    tile = data$raster_collection_tiles[[tile_index]]
    
    
    for (time_index in 1:length(tile$data)) {
      time = NULL
      if (dimensions$time) {
        time = tile$start_times[[time_index]]
      }
      
      band = tile$wavelength
      if (is.null(band)) band = 1
      
      collection$addGranule(time = time,data = tile_to_raster(tile,srs),band=band)
      
    }
    
  }
  
  return(collection)
}
