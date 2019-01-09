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

# provide a function to at offer the file based approach for UDF evaluation
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
