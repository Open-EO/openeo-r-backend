#' @importFrom lubridate as_datetime
#' @export
Service <- R6Class(
  "Service",
  inherit = DatabaseEntity,
  # public ----
  public = list(
    # attributes ====
    # db fields
    service_id = NA,
    job_id = NA,
    title = NA,
    description = NA,
    process_graph = NA, # this should be the graph_id
    type = NA,
    enabled = TRUE,
    plan = NA,
    budget = NA,
    submitted = NA,
    costs = NA,
    
    attributes = list(),
    parameters = list(),
    # TODO remove db field "args"
    
    
    
    # functions ====
    initialize = function(service_id = NULL) {
      self$service_id = service_id
      invisible(self)
    },
    shortInfo = function() {
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
      info = list(
        service_id = self$service_id,
        title = self$title,
        description = self$description,
        url = self$url,
        type = self$type,
        enabled = self$enabled,
        submitted = iso_datetime(self$submitted),
        plan = self$plan,
        costs = self$costs,
        budget = self$budget
      )
      
      return(info)
    }, 
    
    detailedInfo = function () {
      info = list(
        service_id = self$service_id,
        title = self$title,
        description = self$description,
        url = self$url,
        type = self$type,
        enabled = self$enabled,
        submitted = iso_datetime(self$submitted),
        plan = self$plan,
        costs = self$costs,
        budget = self$budget,
        parameters = self$parameters,
        attributes = self$attributes,
        process_graph = self$job$getProcessGraph()$process_graph
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
        
        self$service_id = service_info$service_id
        self$job_id = service_info$job_id
        self$title = service_info$title
        self$description = service_info$description
        self$type = service_info$type
        
        if (!is.na(service_info$parameters)) {
          self$parameters = fromJSON(decodeHex2Char(service_info$parameters),
                                     simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
        } else {
          self$parameters = NA
        }
        
        if (!is.na(service_info$attributes)) {
          self$attributes = fromJSON(decodeHex2Char(service_info$attributes),
                                     simplifyDataFrame = FALSE, simplifyMatrix = FALSE, simplifyVector = FALSE)
        } else {
          self$attributes = NA
        }
        
        
        self$plan = service_info$plan
        self$costs = service_info$costs
        self$budget = service_info$budget
        self$enabled = as.logical(service_info$enabled)
        self$submitted = as_datetime(service_info$submitted)
        
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

        if (!is.null(self$parameters) && (is.list(self$parameters) && length(self$parameters) > 0)) {
          parameters = encodeChar2Hex(toJSON(self$parameters, auto_unbox = FALSE, pretty = TRUE))
        } else {
          parameters = NA
        }
        
        if (!is.null(self$attributes) && (is.list(self$attributes) && length(self$attributes) > 0)) {
          attributes = encodeChar2Hex(toJSON(self$attributes, auto_unbox = FALSE, pretty = TRUE))
        } else {
          attributes=NA
        }
        
        con = openeo.server$getConnection()
        dbExecute(con, insertQuery, param=list(
          sid = self$service_id,
          type = self$type,
          jid = self$job_id,
          parameters = parameters,
          attributes = attributes,
          title = self$title,
          description = self$description,
          plan = self$plan,
          costs = self$costs,
          budget = self$budget,
          enabled = self$enabled,
          submitted = as.character(now())
        ))
        dbDisconnect(con)
        
      } else {
        # update values

        updateQuery = "update service 
          set title = :title,
          description = :description,
          enabled = :enabled,
          job_id = :job_id,
          parameters = :parameters,
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
          parameters = encodeChar2Hex(toJSON(self$parameters, auto_unbox = TRUE, pretty = TRUE)),
          plan = self$plan,
          budget = self$budget
        ))
        dbDisconnect(con)
        
      }
      invisible(self)
    },
    remove = function() {
      service_id = self$service_id
      
      self$job$remove()
      
      con = openeo.server$getConnection()
      deleteQuery = "delete from service where service_id = :sid"
      dbExecute(con, deleteQuery, param=list(sid=service_id))
      dbDisconnect(con)
      
      mapfile = paste(openeo.server$configuration$workspaces.path,"services",paste(service_id,"map",sep="."),sep="/")
      
      if (file.exists(mapfile)) {
        unlink(mapfile)
      }
    },
    buildMapFile = function() {
      mapfile = paste(openeo.server$configuration$workspaces.path,"services",paste(self$service_id,".map",sep=""),sep="/")
      if (file.exists(mapfile)) {
        file.remove(mapfile)
      }
      if (self$type %in% c("wms","wcs")) {
        job_result_path = paste(openeo.server$configuration$workspaces.path,"jobs",self$job$job_id,sep="/")
        files = list.files(job_result_path,pattern="[^process.log|map.map]",full.names = TRUE)
        files = setdiff(files,list.files(job_result_path,pattern="aux.xml",full.names = TRUE))
        config = MapServerConfig$new()
        config = config$fromRaster(obj = lapply(files,brick),service=self)
        #TODO maybe we need to create layers for each additional file...
        
        config$toFile(mapfile)
      } else {
        job_folder = paste(openeo.server$configuration$workspaces.path,"jobs",self$job$job_id,sep="/")
        files = list.files(job_folder,pattern="*.shp",full.names = TRUE)
        if (is.null(files) || length(files) == 0) {
          stop("Cannot find SHP file to create WFS from.")
        }
        
        config = MapServerConfig$new()
        config = config$fromVector(obj = readOGR(files[1]),service=self,data.dir=job_folder)
        
        config$toFile(mapfile)
      }
    }
  ),
  # actives ----
  active = list(
    url = function() {
      return(paste0(openeo.server$configuration$baseserver.url,"/", self$type,"/",self$service_id))
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
    # TODO deprecated?
    argsToString = function(args) {
      text = character()
      
      for (key in names(args)) {
        value = args[[key]]
        text = append(text, paste(key,value,sep="="))
      }
      
      return(paste(text,collapse = ";"))
    },
    # TODO deprecated?
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