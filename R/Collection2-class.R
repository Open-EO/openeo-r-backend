#' Collection
#' 
#' This class represents the collections, which contain a set of Granules. Collections are also used to transfer
#' intermediate results from one process to an other (the result of a process is a Collection).
#' 
#' @field granules A list of Granules that shall be sorted by time ascending
#' @field dimensions A dimensionality object containing information about the existence of dimensions
#' @include Granule-class.R
#' @importFrom R6 R6Class
#' @export
Collection2 <- R6Class(
  "Collection2",
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
      
      private$granules = list()
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
      
      if (self$dimensions$space && is.null(space)) {
        if (!(self$dimensions$feature || self$dimensions$raster)) {
          stop("Dimension space is specified with no default")
        } else {
          space = extent2polygon(extent(data),crs(data))
        }
      }
      
      if (self$dimensions$time && is.null(time) && !class(time) == "POSIXct") {
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
      
      # indices = private$filterbytime(private$granules,from,to)
      
      table = filter(private$data_table, time >= from & time <= to)
      
      res = self$clone(deep=TRUE)
      res$setData(table)
      
      # res$granules = res$granules[indices$min : indices$max]
      return(res)
    },
    filterByBands = function(bands) {
      # bands shall refer to the band indices!
      if (!self$dimensions$band) {
        stop("Cannot select by bands. Collection does not contain a band dimension")
      }
      
      res = self$clone(deep=TRUE)
      # res$granules = private$filterbyband(res$granules,bands)
      res$setData(filter(private$data_table, band %in% bands))
      
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
      
      # globalExtent = private$granules[[1]]$extent
      # 
      # lapply(private$granules, function(g) {
      #   globalExtent <<- union(globalExtent,g$extent)
      # })
      
      return(globalExtent)
    },
    getGlobalSRS = function() {
      if(length(private$granules) >= 1) {
        return(private$granules[[1]]$srs)
      } else {
        return(NULL)
      }
      
    },
    getBandNames = function() {
      firstGranule = private$granules[[1]]
      return(names(firstGranule$bands))
    },
    getBandIndex = function(band_id) {
      return(match(band_id, self$getBandNames()))
    }
  ),
  # private ----
  private = list(
    # attributes ====
    granules = NULL,
    data = NULL, # references to files (complete Raster* or Spatia* object)
    data_table = NULL,
    
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
    },
    
    filterbyband = function (granules,bands) {
      filteredGranules = list()
      for (i in 1:length(granules)) {
        currentGranule = granules[[i]]$clone(deep=TRUE)
        
        bandIndices = currentGranule$getBandIndices(bands)
        currentGranule$data = subset(currentGranule$data, subset = bandIndices)
        currentGranule$bands = currentGranule$bands[bandIndices]
        
        filteredGranules[[i]] = currentGranule    
      }
      
      return(filteredGranules)
    },
    
    filterbytime = function (granules,from,to) {
      minpos = -1
      maxpos = -1
      if (from > to) {
        old <- to
        to <- from
        from <- old
      }
      
      if (length(granules) == 0) return(NULL)
      
      for (i in 1:length(granules)) {
        currentGranule = granules[[i]]
        if (!is.null(currentGranule)) {
          if (currentGranule$time >= from && minpos < 0) {
            minpos = i
          }
          
          if (currentGranule$time > to && maxpos < 0) {
            maxpos = i-1
          }
        }
        if (minpos >= 0 && maxpos >= 0) {
          break
        }
      }
      return(list(min = minpos, max = maxpos))
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