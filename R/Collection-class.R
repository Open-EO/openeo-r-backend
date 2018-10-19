#' Collection
#' 
#' This class represents the collections, which contain a set of Granules. Collections are also used to transfer
#' intermediate results from one process to an other (the result of a process is a Collection).
#' 
#' @field dimensions A dimensionality object containing information about the existence of dimensions
#' @field space A tibble containing an index and the geometry
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
      if (is.null(private$srs) && !is.null(space)) {
        private$srs = crs(space)
      }
      
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
    
    addGranule = function(space=NULL,time=NULL, band=NULL, data, ..., meta_band = NULL, band_file = NULL) {
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
      
      if (self$dimensions$band && !is.null(band) && !is.null(meta_band)) {
        #meta_band is a list, containing band_id and name, or wavelength for all bands
        
        if (is.null(private$bands_metadata)) {
          private$bands_metadata = list()
        }
        
        if (!is.null(band) && (length(private$bands_metadata) < band || is.null(private$bands_metadata[[band]]))) {
          if (is.null(band_file)) {
            filePath = data@file@name
          } else {
            filePath = band_file
          }
          
          
        
          md = GDALinfo(filePath,silent=TRUE)
          scale = attr(md,"ScaleOffset")[1,"scale"]
          offset = attr(md,"ScaleOffset")[1,"offset"]
          type = tolower(attr(md,"df")[1,"GDType"])
          nodata=attr(md,"df")[1,"NoDataValue"]
          resolution = list(x=md["res.x"],y=md["res.y"])
          
          if (!is.null(meta_band$band_id) && length(meta_band$band_id) > 0) {
            band_id = meta_band$band_id[[band]]
          } else {
            band_id = as.character(band)
          }
          
          if (!is.null(meta_band$name) && length(meta_band$name) > 0) {
            name = meta_band$name[[band]]
          } else {
            name = NA
          }
          
          if (!is.null(meta_band$wavelengths) && length(meta_band$wavelengths) > 0) {
            wavelength = meta_band$wavelengths[[band]]
          } else {
            wavelength = NA
          }
          
          bandObj = Band$new(band_id=band_id,
                               name=name,
                               type = type,
                               scale = scale, 
                               offset = offset,
                               nodata=nodata,
                               wavelength_nm = wavelength,
                               res=resolution)
          
          private$bands_metadata[[band]] = bandObj
        }
          
      } 
      
      
      if (!is.null(band) && length(band) >= 1) { #unstack also one banded images
        # add multiple bands
        adding = tibble(band = band)
        if (self$dimensions$time) {
          adding = add_column(adding,time = time)
        }
        
        if (class(data) != "RasterLayer") {
          layer = unstack(data)
        } else {
          layer = list(data)
        }
        
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
      return(sapply(self$getBandsMetadata(), function(band) {return(paste(band$band_id,sep=""))}))
    },
    getBandIndex = function(band_id) {
      return(match(band_id, self$getBandNames()))
    },
    toFile = function(dir=NULL, format=NULL,temp=FALSE,logger) {
      if (is.null(dir)) {
        dir = getwd()
      }
      dir = gsub(pattern = "^(.*[^/])/+$", "\\1",dir) #remove tailing slashes
      # to raster data file ====
      if (self$dimensions$raster) {
        logger$info("Creating raster file with GDAL")
        # collection contains raster data
        if (is.null(format)) {
          format = "GTiff"
        }
        
        # group by time if time is present
        if (self$dimensions$time) {
          # order by bands if present, stack single bands into one file
          
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
                    file.path = paste(dir,file.names[[i]],sep="/")
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
      
      # to vector data file ====
      if (self$dimensions$feature) {
        logger$info("Creating vector file with OGR")
        
        if (is.null(format)) {
          format = "GeoJSON"
        }

        if (temp) {
          file.name = tempfile(tmpdir=dir)
        } else {
          file.name = paste(dir,"/","output",".",.ogrExtension(format),sep="")
        }
        
        if (self$dimensions$time) {
          out = private$data_table %>% dplyr::group_by(space) %>% dplyr::arrange(time) %>% dplyr::summarise(data=tibble(band,time,data) %>% (function(x, ...){
            values = unlist(x$data)
            names = paste(x$band,as.character(x$time), sep=".")
            names(values) = names
            
            return(list(values))
          }))
          
        } else {
          #no time dimension
          out = private$data_table %>% dplyr::group_by(space) %>% dplyr::summarise(data=tibble(band,data) %>% (function(x, ...){
            values = unlist(x$data)
            names = x$band
            names(values) = names
            
            return(list(values))
          }))
        }
        data = as_tibble(t(as.data.frame(out$data)))
        data = data %>% rowid_to_column("ID")
        out_data = self$space %>% inner_join(data,by="ID")
        st_write(out_data,dsn=file.name,driver=format)
        
        private$data_table = private$data_table %>% add_column(output.file=file.name)
        
        return(invisible(self))
      }
      
      # it should be a vector, maybe just write a csv
      stop("Non raster or vector output is not implemented yet")
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

#' Imports a collection
#' 
#' The function uses a predefined lookup table for data retrieval and a metadata json document to 
#' describe the whole collection. 
#' 
#' @details The files are named lookup.csv and md.json. The first has the columns "file_access", "timestamp",
#' "band_index" and "filename". With file_access we state the link to the accessed file how the
#' data will be loaded and accessed, e.g. a VRT. "filename" links to the actual file, which should be a single
#' band. This will be used to extract relevant metadata for each individual band (like the actual spatial 
#' resolution), which might not be correctly obtainable from a VRT.
#' 
#' @importFrom lubridate as_datetime
#' @import dplyr
#' @import tibble
#' @import magrittr
#' @importFrom jsonlite read_json
#' @export
importCollection = function(path,fun=brick) {
  lookup = read.csv2(paste(path,"lookup.csv",sep="/"))
  lookup$timestamp = lubridate::as_datetime(lookup$timestamp)
  lookup = tibble::as_tibble(lookup)
  
  l = lookup %>% 
    dplyr::rowwise() %>% 
    dplyr::do(band_index=as.integer(.$band_index),
       timestamp=as_datetime(.$timestamp),
       data = fun(paste(path,.$file_access,sep="/"))[[.$band_index]],
       filename = .$filename) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(band_index=unlist(band_index),timestamp = as_datetime(unlist(timestamp)),filename = unlist(filename))
  
  col = Collection$new(create_dimensionality(space=TRUE,time=TRUE,band=TRUE,raster=TRUE))
  
  meta = jsonlite::read_json(paste(path,"md.json",sep="/"))
  
  band_ids = names(meta$`eo:bands`)
  names = sapply(meta$`eo:bands`,function(band){
    n = band$common_name
    if (is.null(n)) {
      n = NA_character_
    }
    return(n)
  })
  wavelengths = sapply(meta$`eo:bands`,function(band){
    l = band$center_wavelength
    if (is.null(l)) {
      l = NA_real_
    }
    return(l)
  })
  
  band_meta=tibble(band_id = band_ids, name = names, wavelengths = wavelengths)
  
  added = l %>% rowwise() %>% dplyr::do(done = {
    col$addGranule(band = .$band_index,
                   time = .$timestamp,
                   data = unlist(.$data), 
                   meta_band = band_meta,
                   band_file = paste(path,.$filename,sep="/"))
    TRUE
  })
  
  # update extents with inserted data
  # TODO deal with open temporal intervals 
  e = as(extent(col),"SpatialPolygons")
  crs(e) = crs(col)
  e = spTransform(e, crs("+init=EPSG:4326"))
  meta$extent$spatial = extent(e)[c(1,3,2,4)]
  meta$extent$temporal = c(openEO.R.Backend:::iso_datetime(col$getMinTime()), 
                           openEO.R.Backend:::iso_datetime(col$getMaxTime()))
  
  prod = Product$new(id=meta$name,title = meta$title, description=meta$description)
  prod$setCollectionMetadata(meta)
  prod$setCollection(col)
  prod$deriveMetadata()
  return(prod)
}

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

#' Reads a legend file and creates a Collection
#' 
#' The function uses a legend.csv file as described in 
#' https://github.com/pramitghosh/OpenEO.R.UDF/blob/master/data/example_udf_out/out_legend.csv and a dimensionality code
#' as well as optional additional parameters for band metadata.
#' 
#' @param legend.path Absolute file path to the legend file
#' @param code A dimensionality code or an integer that represents the binary code
#' @param ... contains attributes that are passed on to addGranule as meta_band
#' @return a Collection object
#' 
#' @export
read_legend = function(legend.path,code, ...) {
  dots = list(...)
  
  collection = Collection$new(read_dimensionality(code))
  
  table=as_tibble(read.csv(legend.path,header = TRUE,as.is = TRUE))
  
  if (! "timestamp" %in% names(table) && collection$dimensions$time) {
    stop("Expecting temporal dimension, but none found in input")
  }
  
  if ("timestamp" %in% names(table)) {
    # cast timestamp to POSIXct
    
    table$timestamp = as_datetime(table$timestamp)
  }
  
  # ignoring xmin,xmax,ymin,ymax for now since we do not have a reference system
  
  # prepareBands
  parentFolder = dirname(legend.path)
  
  bandinfo = table %>% group_by(band_index) %>% dplyr::summarise(band = first(band), data = first(filename)) %>% dplyr::arrange(band_index)
  # use band as band_index
  dots$band_id = bandinfo$band
  
  for (i in 1:nrow(table)) {
    row = table[i,]
    if (as.integer(row$whether_raster) == 1) {
      absolute.filepath = paste(parentFolder,row$filename,sep="/")
      collection$addGranule(time=table[i,]$timestamp,band=row$band_index,data=raster(absolute.filepath), meta_band = dots)
    } else {
      stop("Importing a vector feture collection is not yet supported by this function")
    }
  }
  
  
  return(collection)
}

#' @export
setMethod("crs", signature="Collection", function(x, ...) {
  return(crs.Collection(x,...))
})

crs.Collection = function(x, ...) {
  if (x$dimensions$space) {
    return(x$getGlobalSRS())
  } else {
    stop("Try to select a spatial reference system without a spatial dimension defined.")
  }
  
}

is.st_raster = function(x) {
  dims = x$dimensions
  return(dims$space && dims$time && dims$raster)
}
# what about multiband / attribute ?

is.st_feature = function(x) {
  dims = x$dimensions
  return(dims$space && dims$time && dims$feature)
}

is.raster = function(x) {
  dims = x$dimensions
  return(dims$space && dims$raster && !dims$time)
} 

is.feature = function(x) {
  dims = x$dimensions
  return(dims$space && dims$feature && !dims$time)
}

is.timeseries = function(x) {
  dims = x$dimensions
  return(dims$time && !dims$space && !dims$raster && !dims$feature)
}

is.scalar = function(x) {
  dims = x$dimensions
  return(!dims$time && !dims$space && !dims$raster && !dims$feature)
}