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
    
    # functions ====
    initialize = function(dimensions) {
      if (is.na(dimensions) || class(dimensions) != "Dimensionality") {
        stop("Cannot initialize Collection without proper Dimensionality object")
      }
      self$dimensions = dimensions
      
      private$init_tibble()
      private$data = list()
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
    addGranule = function(space=NULL,time=NULL, band=NULL, data, ...) {
      
      dot_args = list(...)
      
      if (is.character(data)) {
        if (self$dimensions$raster) {
          data = brick(data)
        } else if (self$dimensions$feature) {
          data = readOGR(dsn = data, layer = dot_args$layer)
        }
      }
      private$data = append(private$data, data)
      
      if (is.null(private$srs)) {
        private$srs = crs(data)
      }
      
      if (self$dimensions$space && is.null(space)) {
        if (!(self$dimensions$feature || self$dimensions$raster)) {
          stop("Dimension space is specified with no default")
        } else {
          space = extent2polygon(extent(data),crs(data))
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
          adding = add_column(adding,space = lapply(bands,function(band) extent2polygon(extent(band),crs(band))))
        }
        
        private$data_table = rbind(private$data_table,adding)

      } else {
        # add only one entry
        if (!is.list(data)) data = list(data)
        
        adding = tibble(data=data)
        if (self$dimensions$time) {
          adding = add_column(adding,time=time)
        }
        if (!is.list(space)) space = list(space)
        
        if (self$dimensions$space) {
          adding = add_column(adding,space=space)
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
      all_polygons = do.call(bind,private$data_table[["space"]])
      globalExtent = extent(all_polygons)
      
      return(globalExtent)
    },
    getGlobalSRS = function() {
      return(private$srs)
    },
    getBandNames = function() {
      return(names(private$bands_metadata))
    },
    getBandIndex = function(band_id) {
      return(match(band_id, names(self$getBandNames())))
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
            group_by(time) %>% 
            arrange(band) %>% 
            dplyr::summarise(
              space=list(first(space)),
              data= tibble(data) %>% (function(x,...){
                s = stack(x$data)
                return(list(s))
              }))
          }
            
          # at least time and data should be there (probably also space)
          private$data_table = private$data_table %>%
            group_by(time) %>%
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
              arrange(band) %>% 
              dplyr::summarise(
                space=list(first(space)),
                data= tibble(data) %>% (function(x,...){
                  s = stack(x$data)
                  return(list(s))
                }))
          }
          
          private$data_table = private$data_table %>%
            mutate(output.file = tibble(data,space) %>% (
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
                  
                  if (tempfile) {
                    break;
                  }
                }
                if (tempfile) {
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
    data = NULL, # references to files (complete Raster* or Spatial* object)
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
        init = add_column(init, space=list())
      }
      
      if (self$dimensions$band) {
        init = add_column(init, band=integer(0))
      }
      
      init = add_column(init, data=list())
      
      private$data_table = init
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

#' @export
setOldClass(c("Collection", "R6"))

#' @export
setMethod("extent", signature="Collection", function(x, ...) {
  return(x$calculateExtent())
})
