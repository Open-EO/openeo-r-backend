#' Collection
#' 
#' This class represents the collections, which contain a set of Granules. Collections are also used to transfer
#' intermediate results from one process to an other (the result of a process is a Collection).
#' 
#' @field dimensions A dimensionality object containing information about the existence of dimensions
#' @include Granule-class.R
#' @importFrom R6 R6Class
#' @export
Collection <- R6Class(
  "Collection",
  # public ----
  public = list(
    # attributes ====
    dimensions = NULL,
    space = NULL,
    
    # functions ====
    initialize = function(dimensions) {
      if (is.na(dimensions) || class(dimensions) != "Dimensionality") {
        stop("Cannot initialize Collection without proper Dimensionality object")
      }
      self$dimensions = dimensions
      
      private$init_tibble()
    },
    
    getData = function() {
      return(private$data_table)
    },
    setData = function(table) {
      if (!"tbl" %in% class(table)) {
        stop("Cannot set data table")
      }
      private$data_table = table
    },
    setBandsMetadata = function(bands) {
      if (!self$dimensions$band) {
        warning("Setting band metadata without band dimension being present.")
      }
      
      if (is.list(bands)) {
        if (is.null(names(bands))) {
          # create a named list
          names = sapply(bands, function(band) {return(paste(band$band_id,sep=""))})
          names(bands) <- names
        }
      } else if (class(bands)[1] == "Band") {
        band_id = bands$band_id
        bands = list(bands)
        names(bands) <- c(band_id)
      }
      
      private$bands_metadata = bands
    },
    getBandsMetadata = function() {
      return(private$bands_metadata)
    },
    addFeature = function(space=NULL,time=NULL,band=NULL,data,...) {
      # space: spatial features
      # time: time stamps (1D tibble)
      # band: maybe the attribute name
      # data: a data.frame (each row is the spatial feature)
      if (!is.null(band) && length(band) > 1) {
        stop("Cannot process more than one attribute in one addFeature call")
      }
      
      table = NULL
      if (!is.null(time)) {
        # table = tibble(time=time)
        table = time
      }
      
      for (i in 1:length(space)) {
        polygon = polygons(space)[i,]
        spatial_id = private$registerSpace.feature(elem=polygon)
        if (is.null(table)) {
          table = tibble(space=spatial_id)
        } else {
          if (is.null(time)) {
            table=tibble(space=spatial_id)
          } else {
            if (i == 1) {
              table = table %>% add_column(space=i)
            } else {
              table = rbind(table, time %>% add_column(space=spatial_id))
            }
          }
        }
      }
      
      if (!is.null(band)) {
        table = table %>% add_column(band = band)
      }
      
      values= as.numeric(t(data))
      table = table %>% add_column(data=values)
      
      private$data_table = table
      
      invisible(self)
    },
    
    addGranule = function(space=NULL,time=NULL, band=NULL, data, ...) {
      
      dot_args = list(...)
      
      if (is.character(data)) {
        if (self$dimensions$raster) {
          data = brick(data)
        } else if (self$dimensions$feature) {
          data = readOGR(dsn = data, layer = dot_args$layer)
        }
      }
      
      if (is.null(private$srs)) {
        private$srs = crs(data)
      }
      
      if (self$dimensions$space && is.null(space)) {
        if (!(self$dimensions$feature || self$dimensions$raster)) {
          stop("Dimension space is specified with no default")
        } 
      }
      
      if (self$dimensions$time && is.null(time) && class(time) != "POSIXct") {
        stop("Parameter 'time' is missing or is not POSIXct")
      }
      
      if (self$dimensions$band && is.null(band)) {
        if (self$dimensions$raster && nbands(data) > 0) {
          band = 1:nbands(data)
        } else {
          stop("Band dimension is required, but neither a value is given nor can it be derived")
        }
      }
      
      if (!is.null(band) && length(band) > 1) {
        # add multiple bands
        bands = unstack(data)
        adding = tibble(band = band)
        if (self$dimensions$time) {
          adding = add_column(adding,time = time)
        }
        
        layer = unstack(data)
        adding = add_column(adding,data=layer)
        
        if (self$dimensions$space) {
          if (self$dimensions$raster) {
            adding = add_column(adding,space = private$registerSpace.raster(data))
          } else if (self$dimension$feature) {
            adding = add_column(adding,space = private$registerSpace.feature(data))
          }
          
        }
        
        private$data_table = rbind(private$data_table,adding)

      } else {
        # add only one entry
        if (!is.list(data)) data = list(data)
        
        adding = tibble(data=data)
        if (self$dimensions$time) {
          adding = add_column(adding,time=time)
        }
        # if (!is.list(space)) space = list(space)
        
        if (self$dimensions$space) {
          if (self$dimensions$raster) {
            adding = add_column(adding,space=private$registerSpace.raster(data))
          } else if (self$dimensions$feature){
            adding = add_column(adding,space=private$registerSpace.feature(data))
          }
          
        }
        
        if (self$dimensions$band) {
          adding = add_column(adding,band=band)
        }
        
        private$data_table = rbind(private$data_table,adding)
      }
      
    },
    getMinTime = function() {
      if (self$dimensions$time) {
        return(min(private$data_table$time,na.rm=TRUE))
      } else {
        stop("No time dimension declared on data set")
      }
    },
    getMaxTime = function() {
      if (self$dimensions$time) {
        return(max(private$data_table$time,na.rm=TRUE))
      } else {
        stop("No time dimension declared on data set")
      }
    },
    filterByTime = function(from=NA,to=NA) {
      if (is.na(from)) {
        from = self$getMinTime()
      }
      if (is.na(to)) {
        to = self$getMaxTime()
      }
      
      if (from > to) {
        old <- to
        to <- from
        from <- old
      }
      
      table = filter(private$data_table, time >= from & time <= to)
      
      res = self$clone(deep=TRUE)
      res$setData(table)
      
      return(res)
    },
    filterByBands = function(bands) {
      # bands shall refer to the band indices!
      if (!self$dimensions$band) {
        stop("Cannot select by bands. Collection does not contain a band dimension")
      }
      
      res = self$clone(deep=TRUE)
      res$setData(private$data_table %>% dplyr::filter(band %in% bands))
      
      return(res)
    },
    sortGranulesByTime= function () {
      
      if (!self$dimensions$time) {
        stop("Cannot sort by time, no temporal dimension in data collection")
      }
      
      private$data_table = arrange(private$data_table ,time)
    },
    calculateExtent = function() {
      if (!self$dimensions$space) {
        stop("Cannot create an extent, since there are not spatial components")
      }
      # all_polygons = do.call(bind,private$data_table[["space"]])
      # globalExtent = extent(all_polygons)
      
      return(extent(self$space))
    },
    getGlobalSRS = function() {
      return(private$srs)
    },
    getBandNames = function() {
      return(names(private$bands_metadata))
    },
    getBandIndex = function(band_id) {
      return(match(band_id, self$getBandNames()))
    },
    toFile = function(dir=NULL, format=NULL,temp=FALSE) {
      if (is.null(dir)) {
        dir = getwd()
      }
      dir = gsub(pattern = "^(.*[^/])/+$", "\\1",dir) #remove tailing slashes
      if (self$dimensions$raster) {
        # collection contains raster data
        if (is.null(format)) {
          format = "GTiff"
        }
        
        # group by time if time is present
        if (self$dimensions$time) {
          # order by bands if present, stack single bands into one file
          
          # space is fixed (for now)
          # TODO rework the holding of spatial extents in a spatial* data.frame where we store the feature and
          # state the IDs in the table
          if (self$dimensions$band) {
            private$data_table = private$data_table %>% 
            group_by(time,space) %>% 
            arrange(band) %>% 
            dplyr::summarise(
              data= tibble(data) %>% (function(x,...){
                s = stack(x$data)
                return(list(s))
              }))
          }
            
          # at least time and data should be there (probably also space)
          private$data_table = private$data_table %>%
            group_by(time,space) %>%
            mutate(output.file = tibble(time,data,space) %>% (
              function(x, ...) {
                rasters = x$data
                file.names = paste("output",format(x$time,format="%Y%m%dT%H%M%S"),1:length(x$space),sep="_")
                
                for (i in 1:length(rasters)) {
                  if (temp) {
                    file.path = tempfile()
                  } else {
                    file.path = paste(dir,file.names[[1]],sep="/")
                  }
                  
                  # TODO think about "bylayer" in cases formats do not support multilayer 
                  written_file = writeRaster(rasters[[i]],filename=file.path, format=format)
                  file.names[[i]] = written_file@file@name
                  
                  if (temp) break;
                }
                if (temp) {
                  return(file.names[1])
                } else {
                  return(file.names)
                }
                
              }
            )) 
        } else {
          #no time dimension
          if (self$dimensions$band) {
            private$data_table = private$data_table %>% 
              group_by(space) %>% 
              arrange(band) %>% 
              dplyr::summarise(
                data= tibble(data) %>% (function(x,...){
                  s = stack(x$data)
                  return(list(s))
                }))
          }
          
          
          private$data_table = private$data_table %>%
            group_by(space) %>% 
            dplyr::mutate(output.file = tibble(data,space) %>% (
              function(x, ...) {
                rasters = x$data
                file.names = paste("output",1:length(x$space),sep="_")
                
                for (i in 1:length(rasters)) {
                  if (temp) {
                    file.path = tempfile()
                  } else {
                    file.path = paste(dir,file.names[[1]],sep="/")
                  }
                  
                  # TODO think about "bylayer" in cases formats do not support multilayer 
                  written_file = writeRaster(rasters[[i]],filename=file.path, format=format)
                  file.names[[i]] = written_file@file@name
                  
                  if (temp) {
                    break;
                  }
                }
                if (temp) {
                  return(file.names[1])
                } else {
                  return(file.names)
                }
                
              }
            )) 
        } 
          
        return(invisible(self))  
      }
      
      if (self$dimensions$feature) {
        stop("Not implemented yet")
        if (self$dimensions$time) {
          
        } else {
          #no time dimension
        }
        
        return(invisible(self))
      }
      
      # it should be a vector, maybe just write a csv
    }
  ),
  # private ----
  private = list(
    # attributes ====
    srs = NULL,
    data_table = NULL,
    bands_metadata = NULL, # named list of Band
    
    # functions ====
    init_tibble = function() {
      # space contains the spatial extent if applicable
      # time contains POSIXct
      # band contains a discrete number for the band reference
      # data contains the attribute values as either raster* or spatial* objects
      init = tibble()
      
      if (self$dimensions$time) {
        init = add_column(init, time=.POSIXct(integer(0)))
      }
      
      if (self$dimensions$space) {
        init = add_column(init, space=integer(0))
      }
      
      if (self$dimensions$band) {
        init = add_column(init, band=integer(0))
      }
      
      init = add_column(init, data=list())
      
      private$data_table = init
    },
    # stores the spatial component in self$space and returns the spatial ID of the feature in self$space
    registerSpace.raster = function(elem) {
      if (!is.list(elem)) {
        elem = list(elem)
      }
      
      .addSpace = function(elem) {
        crs = crs(elem)
        geom = extent(elem)
        sf_elem = extent2sf(geom,crs)
        
        if (is.null(self$space)) {
          self$space = cbind(sf_elem, ID = 1)
          return(1)
        }
        
        equals = st_equals(self$space,sf_elem)
        elemAlreadyContained = any(as.logical(equals))
        
        if (elemAlreadyContained) {
          index = which(as.logical(equals))
          return(self$space[index,][["ID"]])
        } else {
          lastID = nrow(self$space) + 1
          #add element to spatial with new ID and return ID
          self$space = rbind(self$space, cbind(sf_elem, ID = lastID))
          return(lastID)
        }
      }
      return(sapply(elem,.addSpace))
      
    },
    
    registerSpace.feature = function(elem) {
      # elem is a spatial polygons 
      if (!is.list(elem)) {
        elem = list(elem)
      }
      
      
      .addSpace = function(elem) {
        # crs = crs(elem)
        # geom = extent(elem)
        sf_elem = st_as_sf(elem)
        
        if (is.null(self$space)) {
          self$space = cbind(sf_elem, ID = 1)
          return(1)
        }
        equals = st_equals(self$space,sf_elem)
        elemAlreadyContained = any(as.logical(equals))
        
        if (!is.na(elemAlreadyContained) && elemAlreadyContained) {
          index = which(as.logical(equals))
          return(self$space[index,][["ID"]])
        } else {
          lastID = nrow(self$space) + 1
          #add element to spatial with new ID and return ID
          self$space = rbind(self$space, cbind(sf_elem, ID = lastID))
          return(lastID)
        }
      }
      
      return(sapply(elem,.addSpace))
    }
  )
  
)

# statics ----

is.Collection = function(obj) {
  return("Collection" %in% class(obj))
}

extent2polygon = function(extent,crs) {
  polygon = as(extent,"SpatialPolygons")
  crs(polygon) <- crs
  return(polygon)
}

extent2sf = function(extent, crs) {
  polygon = as(extent,"SpatialPolygons")
  crs(polygon) <- crs
  return(st_as_sf(polygon,crs=crs))
}

#' @export
setOldClass(c("Collection", "R6"))

#' @export
setMethod("extent", signature="Collection", function(x, ...) {
  return(x$calculateExtent())
})
