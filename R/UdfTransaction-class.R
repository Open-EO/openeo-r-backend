#' @export
UdfTransaction <- R6Class(
  "UdfTransaction",
  inherit = DatabaseEntity,
  # public ----
  public = list(
    # attributes ====
    udf_id = NULL,
    job_id = NULL,
    start_date = NULL,
    end_date = NULL,
    status = NULL,
    
    script = NULL,
    logger = NULL,
    
    # functions ====
    initialize = function(udf_id = NA) {
      self$udf_id = udf_id
      
      self$job_id = NA
      self$start_date = NA
      self$end_date = NA
      self$status = NA
      
      invisible(self)
    },
    
    load = function() {
      udf_id = self$udf_id
      
      # check if exists
      if (exists.Udf(udf_id)) {
        # load information from db
        con = openeo.server$getConnection()
        udf_info = dbGetQuery(con, "select * from udf where udf_id = :id"
                                  ,param = list(id=udf_id))
        dbDisconnect(con)
        
        self$udf_id = udf_info$udf_id
        self$job_id = udf_info$job_id
        
        self$start_date = udf_info$start_date
        self$end_date = udf_info$end_date
        self$status = udf_info$status
        
        invisible(self)
      } else {
        stop(paste("Cannot find udf with id:",udf_id))
      }
    },
    
    store = function() {
      
      if (is.na(self$udf_id)) {
        # create new id
        self$udf_id = private$newUdfId()
        self$start_date = format(now(),format="%Y-%m-%d %H:%M:%S")
        
        udf_transaction_folder = self$workspace
        
        if (!dir.exists(udf_transaction_folder)) {
          dir.create(udf_transaction_folder,recursive = TRUE)
        }
        
        results.file.path = self$results_workspace
        if (!dir.exists(results.file.path)) {
          dir.create(results.file.path,recursive = TRUE)
        }

        insertQuery = "insert into udf (
          udf_id, job_id, status, start_date
        ) VALUES (
          :uid, :jid, :status, :start
        )"
        
        tryCatch({
            con = openeo.server$getConnection()
            dbExecute(con, insertQuery, param=list(
              uid = self$udf_id,
              jid = self$job_id,
              status = "started",
              start = self$start_date
            ))
          },
          finally= {
            dbDisconnect(con)
          }
        )
        
        
      } else {
        # update values
        
        updateQuery = "update udf 
          set end_date = :end, status = :status
          where udf_id = :uid
        "
        con = openeo.server$getConnection()
        dbExecute(con, updateQuery, param=list(
          end = self$end_date,
          status = self$status,
          uid = self$udf_id
        ))
        dbDisconnect(con)
        
      }
      invisible(self)
    },
    
    remove = function() {
      udf_id = self$udf_id
      
      con = openeo.server$getConnection()
      deleteQuery = "delete from udf where udf_id = :uid"
      dbExecute(con, deleteQuery, param=list(uid=udf_id))
      dbDisconnect(con)
      
      
      if (dir.exists(self$workspace)) {
        unlink(self$workspace, recursive = TRUE)
      }
    },
    
    clearExportData = function() {
      if (openeo.server$configuration$udf_cleanup) {
        # deletes all export file except the results
        files = list.files(path=self$workspace, recursive = TRUE,full.names = TRUE)
        unlink(files[!grepl("result",files)],recursive = TRUE)
        
        dirs=list.dirs(self$workspace)
        unlink(dirs[!grepl("result",dirs)][-1], recursive = TRUE) # -1 removes the first argument (the transaction folder)
      }
    },
    
    prepareExportData = function(collection, export_type="file") {
      if (is.null(self$logger)) {
        self$logger = Logger$new(process = list(process_id="udf_transaction"),job=list(job_id=self$job_id))
      }
      
      if (!all(export_type %in% c("file","json"))) {
        self$logger$error("Can only support file and json based data export")
      }
      
      if (! is.Collection(collection)) {
        self$logger$error("Passed object is not a Collection object")
      }
      
      if ("json" %in% export_type) {
        json = private$createJsonRequest(collection = collection, strategy = NULL, language = "R")  
        # TODO tiling, coordinate HTTP requests, stitch everything together
        # for now just write to disk
        write(toJSON(json, auto_unbox=TRUE,pretty = TRUE),
              paste(self$workspace,"udf_request.json",sep="/"))
      }
      
      if ("file" %in% export_type) {
        write_generics(collection,dir_name = self$workspace, logger = self$logger)
      }
      
      invisible(self)
    }
  ),
  # active ----
  active = list(
    workspace = function() {
      if (!is.null(self$udf_id)) {
        return(paste(openeo.server$configuration$udf_transactions.path,self$udf_id,sep="/"))
      } else {
        stop("Uninitialized Udf object: no id.")
      }
    },
    results_workspace = function() {
      if (!is.null(self$udf_id)) {
        return(paste(self$workspace,"results",sep="/"))
      } else {
        stop("Uninitialized Udf object: no id.")
      }
    }
  ),
  # private ----
  private = list(
    # attributes ====
    # functions ====
    newUdfId = function() {
      randomString = paste("U",createAlphaNumericId(n=1,length=12),sep="")
      
      
      if (exists.Udf(randomString)) {
        # if id exists get a new one (recursive)
        return(private$newUdfId())
      } else {
        return(randomString)
      }
    },
    
    # Prepares the collection data for the UDF service request
    # 
    # Transforms the data contained in a Collection into a JSON representation. It will be passed along the code script URL as data
    # to the specified UDF REST processing service. Currently implemented only for raster timeserires collections.
    # 
    # @param collection Collection object
    # @param strategy the tiling strategy (not implemented yet)
    # @return list that can be transformed into "UdfData" JSON
    exportCollection.json = function(collection,strategy) {
      # TODO prepare data with some sort of tiling strategy
      
      if (is.st_raster(collection)) {
        udf_data = list()
        udf_data[["proj"]] = as.character(collection$getGlobalSRS())
        udf_data[["raster_collection_tiles"]] = list()
        
        udf_data[["raster_collection_tiles"]] = append(udf_data[["raster_collection_tiles"]],private$raster_collection_export(collection))
        return(udf_data)
      } else {
        stop("Not yet implemented")
      }
      
    },
    
    createJsonRequest = function(collection,strategy=NULL,language="R") {
      # TODO remove the hard coded backend selection
      request = list(
        code = list(
          language = language,
          source = readChar(self$script, file.info(self$script)$size)
        ),
        data = private$exportCollection.json(collection = collection, strategy = strategy)
      )
      
      return(request)
    },
    
    # Creates RasterCollectionTile representation
    # 
    # Subsets and groups Collection data by band and space in order to create the specified UDF RasterCollectionTile JSON output.
    # 
    # @param collection Collection object
    # @return list that can be transformed into "UdfData" JSON
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
  )
)

# statics ====

#' @export
exists.Udf = function(udf_id) {
  if (nchar(udf_id) == 13) {
    con = openeo.server$getConnection()
    result = dbGetQuery(con, "select count(*) from udf where udf_id = :id"
                        ,param = list(id=udf_id)) == 1
    dbDisconnect(con)
    return(result)
  } else {
    return(FALSE)
  }
}

#' @export
is.Udf = function(obj) {
  return("Udf" %in% class(obj))
}

udfIdsByJobId = function(jobid) {
  tryCatch({
    query = "select udf_id from udf where job_id = :jid"
    
    db = openeo.server$getConnection()
    result = dbGetQuery(db, query, param = list(jid=jobid))[,1]
    return(result)
    
  },finally = {
    dbDisconnect(db)
  })
}

removeJobsUdfData = function(job) {
  if (openeo.server$configuration$udf_cleanup) {
    udfids = udfIdsByJobId(job$job_id)
    
    if (length(udfids)>0) {
      for (id in udfids) {
        udf = UdfTransaction$new()
        udf$udf_id = id
        udf$load()
        udf$remove()
        udf=NULL
      }
    }
  }
}

prepare_udf_transaction = function(user,script,job_id = NULL) {
  udf_transaction = UdfTransaction$new()
  
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
  
  udf_transaction$script = file.path
  
  if (!is.null(job_id)) {
    udf_transaction$job_id = job_id
  } else {
    udf_transaction$job_id = "sync_job"
  }
  
  udf_transaction$store()
  
  return(udf_transaction)
}