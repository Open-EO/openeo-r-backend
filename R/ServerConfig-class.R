#' Server Configuration
#' 
#' The server configurations
#' 
#' @field baseserver.url The URL of where this server is accesible. Trailing slashes are removed.
#' 
#' @export
ServerConfig = function() {
  default = list(
    api.version = "0.3.1",
    secret.key = NULL,
    
    data.path = NULL,
    workspaces.path = NULL,
    sqlite.path = NULL,
    
    udf_transactions.path = NULL,
    udf_cleanup = TRUE,
    
    api.port = NULL,
    host = NULL, #TODO check if deprecated
    baseserver.url = "http://localhost:8000",
    mapserver.url = NULL, #assuming here a url, if not specified the backend is probably started with docker-compose
    oidcprovider.url = NULL,
    rudfservice.url = NULL,
    
    outputGDALFormats = NULL,
    defaultRasterFormat = "GTiff",
    outputOGRFormats = NULL,
    defaultVectorFormat = "GeoPackage")
  
  class(default) = "ServerConfig"
  return(default)
}