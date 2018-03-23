#' @export
MapServerConfig <- R6Class(
  "MapServerConfig",
  # public ----
  public=list(
    # attributes ====
    map = NULL,
    
    # functions ====
    initialize = function(obj,service, scale="AUTO", normalize="AUTO") {
      
      service_id = service$service_id
      service_type = service$service_type
      
      map = MapServerMap$new()
      
      if (!is.null(service_id)) {
        map$NAME = service_id
      }
      
      service_type = tolower(service_type)
      if (service_type %in% private$supported_services) {
        map$WEB = private$createWebElement(service_type)
        
        if (service_type %in% c("wms","wcs")) {
          # TODO obj might be a collection...
          
          # assume obj to be a raster for now
          e = extent(obj)
          extent.string = paste(e@xmin, e@ymin, e@xmax, e@ymax)
          map$EXTENT = extent.string
          
          size.string = paste(ncol(obj),nrow(obj))
          map$SIZE = size.string
          
          map$PROJECTION = toString(crs(obj))
          
          # layers
          layers = list()
          
          # bands
          for (band_index in 1:nbands(obj)) {
            band = obj[[band_index]]
            
            layer = MapServerLayer$new()
            layer$NAME = paste("band",band_index,sep="")
            layer$STATUS = "ON"
            layer$TYPE = "RASTER"
            # on the mapserver service the workspace will be found at another directory, so replace it
            layer$DATA = sub(pattern=openeo.server$workspaces.path, replacement="/maps", x=obj@file@name)
            
            processings = list()
            if (!is.null(normalize)) {
              processings = append(processings, paste("KERNELDENSITY_NORMALIZATION",normalize,sep="="))
            }
            if (!is.null(scale)) {
              processings = append(processings, paste("SCALE",scale,sep="="))
            }
            
            processings = append(processings, paste("BANDS",paste(rep(band_index,3),sep=","),sep="="))
            layer$PROCESSING = processings
            
            layers = append(layers,layer)
          }
          
          #TODO maybe add composites like false colors
          
        }
        
      } else {
        stop("Not supported service type.")
      }
      
      map$LAYER = layers
      self$map = map
      
    },
    
    toFile = function(file) {
      
      data = c()
      
      dir = regmatches(file,regexpr(text=file,pattern="^(.*)/"))
      if (!dir.exists(file)) {
        dir.create(dir,recursive = TRUE)
      }
      
      if (!file.exists(file)) {
        file.create(file)
      }
      
      # write stuff to file
      data = self$map$toString()
      
      con = file(file)
      writeLines(data,con)
      close(con)
    }
    
  ),
  # private ----
  private=list(
    # attributes ====
    supported_services = c("wcs","wms","wfs"),
    # functions ====
    createWebElement = function(service_type) {
      web = MapServerWeb$new()
      
      keys = c(paste(service_type,"_srs",sep=""),
               paste(service_type,"_enable_request",sep=""))
      values = list("EPSG:3857 EPSG:4326", "*")
      
      names(values) <- keys
      web$METADATA <- values
      
      return(web)

    }
  )
)

# statics ----

#' Checks an object is a MapServerConfig
#' 
#' Calls the class operation on the object and checks if it is of type MapServerConfig
#' 
#' @param obj an object to be checked
#' @return logical if it is a MapServerConfig object
#' @export
is.MapServerConfig = function(obj) {
  return("MapServerConfig" %in% class(obj))
}

