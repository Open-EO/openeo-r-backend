#' @export
MapServerConfig <- R6Class(
  "MapServerConfig",
  # public ----
  public=list(
    # attributes ====
    map = NULL,
    service = NULL,
    
    # functions ====
    initialize = function() {

    },
    
    toFile = function(file) {
      
      data = c()
      
      dir = regmatches(file,regexpr(text=file,pattern="^(.*)/"))
      if (!dir.exists(dir)) {
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
    },
    
    fromVector = function(obj,service,data.dir=NULL) {
      self$service = service
      service_id = service$service_id
      service_type = service$service_type
      
      map = MapServerMap$new()
      
      if (!is.null(service_id)) {
        map$NAME = service_id
      }
      
      service_type = tolower(service_type)
      if (service_type %in% private$supported_services) {
        map$WEB = private$createWebElement()
        
        if (service_type %in% c("wfs")) {

          e = extent(obj)
          extent.string = paste(e@xmin, e@ymin, e@xmax, e@ymax)
          map$EXTENT = extent.string
          
          if (is.null(crs(obj)) || is.na(crs(obj))) {
            stop("Cannot set an unknown CRS for WFS")
          } else {
            map$PROJECTION = toString(crs(obj))
          }
          
          layers = list()
          
          #for now create only one layer, assuming that this will be the case...
          layer = MapServerLayer$new()
          
          if (regexpr(class(obj),pattern="^.*(Polygon|polygon|POLYGON).*$") > 0) {
            layer$TYPE = "POLYGON"
            layer$NAME = "POLYGON"
          } else if (regexpr(class(obj),pattern="^.*(Line|line|LINE).*$") > 0) {
            layer$TYPE = "LINE"
            layer$NAME = "LINE"
          } else if (regexpr(class(obj),pattern="^.*(Point|point|POINT).*$") > 0) {
            layer$TYPE = "POINT"
            layer$NAME = "POINT"
          } else {
            stop("Currently only supporting polygon, lines or points as data types")
          }
          
          layer$STATUS = "ON"
          
          if (!is.null(data.dir)) {
            # TODO consider multiple files and possibly more than one layer
            layer$CONNECTIONTYPE = "OGR"
            
            # shp files
            vector.files = list.files(data.dir,pattern="*.shp", full.names = TRUE)
            
            if (is.null(vector.files) || length(vector.files) == 0) {
              stop("Cannot find SHP file to create a WFS from")
            }
            
            layer$CONNECTION = sub(pattern=openeo.server$workspaces.path, replacement="/maps", x=gsub("\\\\","/",vector.files[1]))
            layer$DATA= "0"
          }
          
          layer$PROJECTION = toString(crs(obj))
          
          layers = append(layers, layer)
          
        } else {
          stop("Unsupported map file creation for image service from vector data")
        }
        
      } else {
        stop("Not supported service type.")
      }
      
      map$LAYER = layers
      self$map = map
      
      invisible(self)
    },
    
    fromRaster = function(obj,service, scale="AUTO", normalize="AUTO") {
      self$service = service
      service_id = service$service_id
      service_type = service$service_type
      
      map = MapServerMap$new()
      
      if (!is.null(service_id)) {
        map$NAME = service_id
      }
      
      service_type = tolower(service_type)
      if (service_type %in% private$supported_services) {
        map$WEB = private$createWebElement()
        
        if (service_type %in% c("wms","wcs")) {
          #create global variables...
          # assume obj to be a raster for now
          e = extent(obj[[1]])
          extent.string = paste(e@xmin, e@ymin, e@xmax, e@ymax)
          map$EXTENT = extent.string
          
          size.string = paste(ncol(obj[[1]]),nrow(obj[[1]]))
          map$SIZE = size.string
          
          map$PROJECTION = toString(crs(obj[[1]]))
          
          # layers
          layers = list()
          
            # TODO obj might be a collection...
          for (index in 1:length(obj)) {
            ras = obj[[index]]
  
            # bands
            for (band_index in 1:nbands(ras)) {
              band = ras[[band_index]]
              
              layer = MapServerLayer$new()
              layer$NAME = names(band)
              layer$STATUS = "ON"
              layer$TYPE = "RASTER"
              # on the mapserver service the workspace will be found at another directory, so replace it
              layer$DATA = sub(pattern=openeo.server$workspaces.path, replacement="/maps", x=gsub("\\\\","/",ras@file@name))
              
              processings = list()
              if (!is.null(normalize)) {
                processings = append(processings, paste("KERNELDENSITY_NORMALIZATION",normalize,sep="="))
              }
              if (!is.null(scale)) {
                processings = append(processings, paste("SCALE",scale,sep="="))
              }
              
              processings = append(processings, paste("BANDS",paste(rep(band_index,3),sep="",collapse = ","),sep="="))
              layer$PROCESSING = processings
              
              layers = append(layers,layer)
            }
          }
          
          #TODO maybe add composites like false colors
          
        } else {
          stop("Unsupported map file creation for WFS from image object.")
        }
        
      } else {
        stop("Not supported service type.")
      }
      
      map$LAYER = layers
      self$map = map
      
      invisible(self)
    }
    
  ),
  # private ----
  private=list(
    # attributes ====
    supported_services = c("wcs","wms","wfs"),
    # functions ====
    createWebElement = function() {
      service_type = tolower(self$service$service_type)
      web = MapServerWeb$new()
      
      service_url = paste(self$service$url,"?SERVICE=",toupper(service_type),sep="")
      
      version = self$service$service_args[[match("version",tolower(names(self$service$service_args)))]]
      if (!is.null(version)) {
        service_url = paste(service_url,"&VERSION=",version,sep="")
      }
      
      if (tolower(service_type) == "wms") {
        service_operations = "GetCapabilities GetMap"
      } else {
        service_operations = "*"
      }
      
      keys = c(paste(service_type,"_srs",sep=""),
               paste(service_type,"_enable_request",sep=""),
               paste(service_type,"_onlineresource",sep=""),
               paste(service_type,"_title",sep=""))
      values = list("EPSG:3857 EPSG:4326", service_operations , service_url, service_type)
      
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

