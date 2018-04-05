#' @export
Service <- R6Class(
  "Service",
  # public ----
  public = list(
    # attributes ====
    service_id = NULL,
    service_url = NULL,
    service_type = "",
    service_args = "",
    job_id = NULL,
    
    # functions ====
    initialize = function() {
      invisible(self)
    },
    
    detailedInfo = function () {
      info = list(
        service_id = self$service_id,
        service_type = self$service_type,
        service_args = self$service_args,
        job_id = self$job_id,
        service_url = self$url
      )
      
      return(info)
    },
    
    load = function(service_id) {
      # check if exists
      if (exists.service(service_id)) {
        # load information from db
        con = openeo.server$getConnection()
        service_info = dbGetQuery(con, "select * from service where service_id = :id"
                              ,param = list(id=service_id))
        dbDisconnect(con)
        
        self$service_id = service_info$service_id
        self$job_id = service_info$job_id
        self$service_args = private$stringToArgs(service_info$args)
        self$service_type = service_info$type
        
        invisible(self)
      } else {
        stop(paste("Cannot find service with id:",service_id))
      }
      
      
    },
    
    store = function() {
      if (is.null(self$service_id)) {
        # create new id
        self$service_id = private$newServiceId()
        
        insertQuery = "insert into service (
          service_id, type, job_id, args
        ) VALUES (
          :sid, :type, :jid, :args
        )"
        
        con = openeo.server$getConnection()
        dbExecute(con, insertQuery, param=list(
          sid = self$service_id,
          type = self$service_type,
          jid = self$job_id,
          args = private$argsToString(self$service_args)
        ))
        dbDisconnect(con)
        
      } else {
        # update values
        
        updateQuery = "update service 
          set args = :args
          where service_id = :sid
        "
        con = openeo.server$getConnection()
        dbExecute(con, updateQuery, param=list(
          sid = self$service_id,
          args = private$argsToString(self$sevice_args)
        ))
        dbDisconnect(con)
        
      }
      invisible(self)
    }
  ),
  active = list(
    url = function() {
      return(paste("http://",openeo.server$host,":",openeo.server$api.port,"/api/",
                   self$service_type,"/",self$service_id,sep=""))
    }
  ),
  # private ----
  private = list (
    # functions ====
    newServiceId = function() {
      randomString = paste("S",createAlphaNumericId(n=1,length=12),sep="")
      
      
      if (exists.service(randomString)) {
        # if id exists get a new one (recursive)
        return(private$newServiceId())
      } else {
        return(randomString)
      }
    },
    
    argsToString = function(args) {
      text = character()
      
      for (key in names(args)) {
        value = args[[key]]
        text = append(text, paste(key,value,sep="="))
      }
      
      return(paste(text,collapse = ";"))
    },
    
    stringToArgs = function(string) {
      kvp = unlist(strsplit(string,"[=|;]"))
      args = list()
      
      for (key_index in seq(from=1,to=length(kvp),by=2)) {
        args[[kvp[[key_index]]]] <- kvp[key_index + 1]
      }
      
      return(args)
    }
  )
)

# statics ====

#' @export
exists.service = function(service_id) {
  if (nchar(service_id) == 13) {
    con = openeo.server$getConnection()
    result = dbGetQuery(con, "select count(*) from service where service_id = :id"
                        ,param = list(id=service_id)) == 1
    dbDisconnect(con)
    return(result)
  } else {
    return(FALSE)
  }
}

#' @export
is.Service = function(obj) {
  return("Service" %in% class(obj))
}