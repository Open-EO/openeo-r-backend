#' @export
Service <- R6Class(
  "Service",
  inherit = DatabaseEntity,
  # public ----
  public = list(
    # attributes ====
    service_url = NULL,
    
    # db fields
    service_id = NA,
    job_id = NA,
    title = NA,
    description = NA,
    process_graph = NA, # this should be the graph_id
    type = NA,
    enabled = FALSE,
    plan = NA,
    budget = NA,
    submitted = NA,
    costs = NA,
    
    attributes = list(),
    parameters = list(),
    
    
    
    # functions ====
    initialize = function(service_id = NULL) {
      self$service_id = service_id
      invisible(self)
    },
    
    detailedInfo = function () {
      info = list(
        service_id = self$service_id,
        type = self$type,
        parameters = self$parameters,
        job_id = self$job_id,
        service_url = self$url
      )
      
      return(info)
    },
    
    load = function() {
      service_id = self$service_id
      
      # check if exists
      if (exists.Service(service_id)) {
        # load information from db
        con = openeo.server$getConnection()
        service_info = dbGetQuery(con, "select * from service where service_id = :id"
                              ,param = list(id=service_id))
        dbDisconnect(con)
        
        # service_id text,
        # job_id text,
        # title text,
        # description text,
        # type text,
        # parameters text,
        # attributes text,
        # plan text,
        # costs real,
        # budget real,
        # enabled integer,
        # submitted datetime
        
        self$service_id = service_info$service_id
        self$job_id = service_info$job_id
        self$title = service_info$title
        self$description = service_info$description
        self$type = service_info$type
        self$parameters = fromJSON(decodeHex2Char(service_info$parameters),
                                   simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
        self$attributes = fromJSON(decodeHex2Char(service_info$attributes),
                                   simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
        self$plan = service_info$plan
        self$costs = service_info$costs
        self$budget = service_info$budget
        self$enabled = as.logical(service_info$enabled)
        self$submitted = service_info$submitted
        
        
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
          service_id, job_id, title, description, type, parameters, attributes, plan, costs, budget, enabled, submitted
        ) VALUES (
          :sid, :jid, :title, :description, :type, :parameters, :attributes, :plan, :costs, :budget, :enabled, :submitted
        )"
        
        con = openeo.server$getConnection()
        dbExecute(con, insertQuery, param=list(
          sid = self$service_id,
          type = self$type,
          jid = self$job_id,
          parameters = encodeChar2Hex(toJSON(self$parameters, auto_unbox = FALSE, pretty = TRUE)),
          attributes = encodeChar2Hex(toJSON(self$attributes, auto_unbox = FALSE, pretty = TRUE)),
          title = self$title,
          description = self$description,
          plan = self$plan,
          costs = self$costs,
          budget = self$budget,
          enabled = self$enabled,
          submitted = now()
        ))
        dbDisconnect(con)
        
      } else {
        # update values

        updateQuery = "update service 
          set title = :title,
          description = :description,
          enabled = :enabled,
          job_id = :job_id,
          parameter = :parameters,
          plan = :plan,
          budget = :budget
          where service_id = :sid
        "
        con = openeo.server$getConnection()
        dbExecute(con, updateQuery, param=list(
          sid = self$service_id,
          title = self$title,
          description = self$description,
          enabled = self$enabled,
          job_id = self$job_id,
          parameter = encodeChar2Hex(toJSON(self$parameters, auto_unbox = TRUE, pretty = TRUE)),
          plan = self$plan,
          budget = self$budget
        ))
        dbDisconnect(con)
        
      }
      invisible(self)
    },
    remove = function() {
      service_id = self$service_id
      
      con = openeo.server$getConnection()
      deleteQuery = "delete from service where service_id = :sid"
      dbExecute(con, deleteQuery, param=list(sid=service_id))
      dbDisconnect(con)
      
      mapfile = paste(openeo.server$workspaces.path,"services",paste(service_id,"map",sep="."),sep="/")
      
      if (file.exists(mapfile)) {
        unlink(mapfile)
      }
    }
  ),
  # actives ----
  active = list(
    url = function() {
      return(paste("http://",openeo.server$host,":",openeo.server$api.port,"/api/",
                   self$type,"/",self$service_id,sep=""))
    },
    job = function() {
      if (!is.na(self$job_id)) {
        job = Job$new(job_id = self$job_id)
        job$load()
        return(job)
      } else {
        return(NULL)
      }
    }
  ),
  # private ----
  private = list (
    # functions ====
    newServiceId = function() {
      randomString = paste("S",createAlphaNumericId(n=1,length=12),sep="")
      
      
      if (exists.Service(randomString)) {
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
      if (length(kvp > 0)) {
        for (key_index in seq(from=1,to=length(kvp),by=2)) {
          args[[kvp[[key_index]]]] <- kvp[key_index + 1]
        }
      }
      
      
      return(args)
    }
  )
)

# statics ====

#' @export
exists.Service = function(service_id) {
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