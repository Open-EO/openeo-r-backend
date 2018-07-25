#' @export
Udf <- R6Class(
  "Udf",
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
      if (openeo.server$udf_cleanup) {
        # deletes all export file except the results
        files = list.files(path=self$workspace, recursive = TRUE,full.names = TRUE)
        unlink(files[!grepl("result",files)],recursive = TRUE)
        
        dirs=list.dirs(self$workspace)
        unlink(dirs[!grepl("result",dirs)][-1], recursive = TRUE) # -1 removes the first argument (the transaction folder)
      }
    }
  ),
  # active ----
  active = list(
    workspace = function() {
      if (!is.null(self$udf_id)) {
        return(paste(openeo.server$udf_transactions.path,self$udf_id,sep="/"))
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