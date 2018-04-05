#' Job
#' 
#' This class represents an openEO job. Which is submitted by an user, containing an executable process graph. It is strongly bound
#' to the backend, since the OpenEOServer class needs to be accessible, when it comes to loading related process for the process 
#' graph.
#' 
#' @field job_id The unique identifier of the job
#' @field status The current status in the job lifecycle
#' @field process_graph graph of nested processes that is executable
#' @field view Spatio-Temporal Extent to be used for the calculation
#' @field submitted Timestamp when the job was submitted to the server
#' @field user_id The user who owns the job
#' @field consumed_credits For accounting and billing the amount of credits consumed by this job
#' @field filePath The system filepath that links to the stored JSON process graph
#' 
#' @include Process-class.R
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @export
Job <- R6Class(
  "Job",
  # public ====
  public = list(
    # attributes ----
    job_id = NULL,
    status=NULL,
    process_graph = NULL,
    view=NULL,
    submitted=NULL,
    last_update=NULL,
    user_id=NULL,
    consumed_credits=NULL,
    output=NULL,
    results = NULL,
    persistent = FALSE,
    
    # functions ----
    initialize = function(job_id=NULL,process_graph=NULL,user_id = NULL) {
      if (is.null(job_id)||missing(job_id)) {
        stop("Cannot create new Job. There is no job_id specified")
      } else {
        self$job_id = job_id
      }
      
      if (!is.null(user_id)) {
        self$user_id = user_id
      }
      
      self$consumed_credits = 0
      
      if (!is.null(process_graph)) {
        self$process_graph = process_graph
      }
      return(self)
    },
    
    store = function() {
      
      con = openeo.server$getConnection()
      exists = dbGetQuery(con,"select count(*) from job where job_id = :id",param=list(id=self$job_id)) == 1
      if (isProcess(self$process_graph)) {
        stop("Cannot store process_graph. For the database it has to be a key")
      } else if (is.list(self$process_graph)) {
        graph = ProcessGraph$new(self$process_graph, self$user_id)
        graph$store()

        self$process_graph = graph$graph_id
      }
      
      if (!exists) {
        insertIntoQuery = "insert into job (job_id, 
        user_id, 
        status, 
        process_graph,
        submitted,
        last_update,
        consumed_credits) values (
        :job_id, :user_id, :status, :process_graph, :submitted, :last_update, :consumed_credits 
        );"
        dbExecute(con, insertIntoQuery, param = list(
          job_id = self$job_id,
          user_id = self$user_id,
          status = self$status,
          process_graph = self$process_graph,
          submitted=as.character(self$submitted),
          last_update = as.character(self$last_update),
          consumed_credits = self$consumed_credits
        ))

      } else {
        updateQuery = "update job 
                        set 
                          user_id = :id, 
                          status = :status,
                          process_graph = :process_graph, 
                          submitted = :submitted, 
                          last_update = :last_update,
                          consumed_credits = :consumed_credits 
                        where job_id = :job_id;"
        
        dbExecute(con, updateQuery, param = list(
          user_id = self$user_id,
          status = self$status,
          process_graph = self$process_graph,
          submitted = as.character(self$submitted),
          last_update = as.character(self$last_update),
          consumed_credits = self$consumed_credits,
          job_id = self$job_id
        ))
      }
      
      dbDisconnect(con)
      invisible(self)
    },
    
    loadProcessGraph = function() {
      if (!isProcess(self$process_graph)) {
        
        if (is.character(self$process_graph)) {
          if (validate(self$process_graph) == TRUE) {
            parsedJson = fromJSON(self$process_graph, simplifyDataFrame = FALSE)
            if (!"process_graph" %in% names(parsedJson)) {
              parsedJson = list(process_graph=parsedJson)
            }
          } else {
            #should be a process_graph id
            # TODO load process_graph by id
            graph = ProcessGraph$new()
            graph$graph_id = self$process_graph
            graph$load()
            parsedJson = graph$process_graph
          }
        } else if (is.list(self$process_graph)) {
          if (!"process_graph" %in% names(self$process_graph)) {
            parsedJson = list(process_graph=self$process_graph)
          } else {
            parsedJson = self$process_graph
          }
        } else {
          # else it is assumed to be stored as plain text in job.process_graph ?! 
          # - probably does not make sense
          # - never used ?!
          con = openeo.server$getConnection()
          jsonText = dbGetQuery(con, "select process_graph from job where job_id = :id", param=list(id=self$job_id))[1,]
          dbDisconnect(con)
          
          parsedJson = fromJSON(jsonText, simplifyDataFrame = FALSE)
          if (!"process_graph" %in% names(parsedJson)) {
            parsedJson = list(process_graph=self$process_graph)
          }
        }
        
        self$output = parsedJson[["output"]] # NULL if not exists
        
        self$process_graph = self$loadProcess(parsedJson[["process_graph"]])
      } else {
        # do nothing, we already have a process graph
      }
    },

    loadProcess = function(parsedJson) {
      processId = parsedJson[["process_id"]]
      #TODO: add cases for udfs
      if (!is.null(processId) && processId %in% names(openeo.server$processes)) {
        process = openeo.server$processes[[processId]]
        
        return(process$as.executable(parsedJson,self))
      } else {
        stop(paste("Cannot load process",processId))
      }
    },
    shortInfo = function() {
      info = list(
        job_id = self$job_id,
        status = self$status,
        submitted = self$submitted,
        updated = self$last_update,
        consumed_credits = self$consumed_credits
      )
      
      return(info)
    },
    
    detailedInfo = function() {
      if (is.null(self$process_graph) || ! isProcess(self$process_graph)) {
        self$loadProcessGraph()
      }
      
      #process_graph and the nested processes are always ExecutableProcess
      processGraphList = self$process_graph$detailedInfo()
      
      info = list(
        job_id = self$job_id,
        user_id = self$user_id,
        status = self$status,
        process_graph = processGraphList,
        output = self$output,
        submitted = self$submitted,
        updated = self$last_update,
        consumed_credits = self$consumed_credits
      )
      
      return(info)
    },
    
    run = function() {
      if (!isProcess(self$process_graph)) {
          self$loadProcessGraph()
      }
      
      
      tryCatch({
        cat("Start job processing...\n")
        self$status = "running"
        
        if (self$persistent) {
          con = openeo.server$getConnection()
          updateJobQuery = "update job set last_update = :time, status = :status where job_id = :job_id"
          dbExecute(con, updateJobQuery ,param=list(time=as.character(Sys.time()),
                                                    status="running",
                                                    job_id=self$job_id))
          dbDisconnect(con)
        }
        
        
        self$results = self$process_graph$execute()
        
        
        self$status = "finished"
        
        if (self$persistent) {
          con = openeo.server$getConnection()
          updateJobQuery = "update job set last_update = :time, status = :status where job_id = :job_id"
          dbExecute(con, updateJobQuery ,param=list(time=as.character(Sys.time()),
                                                    status="finished",
                                                    job_id=self$job_id))
          dbDisconnect(con)
        }
        
        cat("Job done\n")
      }, error=function (e) {
        cat("Error. Aborting execution.\n")
        self$status = "error"
        if (self$persistent) {
          con = openeo.server$getConnection()
          updateJobQuery = "update job set last_update = :time, status = :status where job_id = :job_id"
          dbExecute(con, updateJobQuery ,param=list(time=as.character(Sys.time()),
                                                    status="error",
                                                    job_id=self$job_id))
          dbDisconnect(con)
        }
      }, finally={
        return(self)
      })
    }
    
  )
)

# statics ====
isJob = function(obj) {
  return("Job" %in% class(obj))
}