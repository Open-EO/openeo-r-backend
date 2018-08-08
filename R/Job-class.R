#' Job
#' 
#' This class represents an openEO job. Which is submitted by an user, containing an executable process graph. It is strongly bound
#' to the backend, since the OpenEOServer class needs to be accessible, when it comes to loading related process for the process 
#' graph.
#' 
#' @field job_id The unique identifier of the job
#' @field status The current status in the job lifecycle
#' @field process_graph graph of nested processes that is executable (ExecutableProcess)
#' @field view Spatio-Temporal Extent to be used for the calculation (currently not in use)
#' @field submitted Timestamp when the job was submitted to the server
#' @field user_id The user who owns the job
#' @field consumed_credits For accounting and billing the amount of credits consumed by this job
#' @field last_update Timestamp when the job was last updated
#' @field results Contains the result of the process_graph after execution (Collection)
#' @field persistent Whether or not the job is stored in database
#' @field output list containing the output configuration like format (or additional GDAL commands)
#' 
#' @include Process-class.R
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate as_datetime
#' @export
Job <- R6Class(
  "Job",
  inherit = DatabaseEntity,
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
    budget=NULL,
    title= NULL,
    description = NULL,
    plan = NULL,
    
    results = NULL, # contains the results of the process_graph after execution
    persistent = FALSE, # whether or not the job is stored in data base
    
    # functions ----
    initialize = function(job_id=NULL,process_graph=NULL,user_id = NULL) {
      self$job_id = job_id
      
      if (!is.null(user_id)) {
        self$user_id = user_id
      }
      
      self$consumed_credits = 0
      if (!is.null(process_graph)) {
        if (!is.ProcessGraph(process_graph)) {
          if (is.graph_id(process_graph)) {
            #load graph id and overwrite user and grpah_id
            private$pg = ProcessGraph$new(graph_id=process_graph)
            private$pg$user_id = user_id
            private$pg$graph_id = NULL # will be created on store
          } else {
            private$pg = ProcessGraph$new(process_graph = process_graph, user_id = user_id)
          }
        } else {
          private$pg$process_graph = process_graph
        }

        user = User$new()$load(user_id=user_id)
        self$process_graph = private$pg$buildExecutableProcessGraph(user=user,job=self)
      } 
      return(self)
    },
    
    store = function() {
      if (is.null(self$job_id)) {
        self$job_id = private$newJobId()
      }
      
      if (is.na(private$pg$graph_id) || is.null(private$pg$graph_id)) {
        private$pg$store()
      }
      
      encodedOutput = NA
      
      if (!is.null(self$output)) {
        encodedOutput = encodeChar2Hex(toJSON(x=self$output,auto_unbox = TRUE,pretty = TRUE))
      }
      
      if (!exists.Job(self$job_id)) {
        
        insertIntoQuery = "insert into job (
            job_id, 
            user_id, 
            status, 
            process_graph,
            submitted,
            last_update,
            consumed_credits,
            output,
            budget,
            title,
            description,
            plan
        ) values (
            :job_id, 
            :user_id, 
            :status, 
            :process_graph, 
            :submitted, 
            :last_update, 
            :consumed_credits,
            :output,
            :budget,
            :title,
            :description,
            :plan
        );"
        
        con = openeo.server$getConnection()
        dbExecute(con, insertIntoQuery, param = list(
          job_id = self$job_id,
          user_id = self$user_id,
          status = self$status,
          process_graph = private$pg$graph_id, # it is the graph_id at this point
          submitted=as.character(self$submitted),
          last_update = as.character(self$last_update),
          consumed_credits = self$consumed_credits,
          output = encodedOutput,
          budget = self$budget,
          title = self$title,
          description = self$description,
          plan = self$plan
        ))
        dbDisconnect(con)

      } else {
        updateQuery = "update job 
                       set 
                          status = :status,
                          submitted = :submitted, 
                          last_update = :last_update,
                          consumed_credits = :consumed_credits,
                          output = :output,
                          budget = :budget,
                          title = :title,
                          description = :description,
                          plan = :plan
                       where 
                          job_id = :job_id;"
        
        con = openeo.server$getConnection()
        dbExecute(con, updateQuery, param = list(
          status = self$status,
          submitted = as.character(self$submitted),
          last_update = as.character(self$last_update),
          consumed_credits = self$consumed_credits,
          job_id = self$job_id,
          output = encodedOutput,
          budget = self$budget,
          title = self$title,
          description = self$description,
          plan = self$plan
        ))
        dbDisconnect(con)
      }
      
      self$persistent = TRUE
      invisible(self)
    },

    load = function() {
      if (is.null(self$job_id)) {
        stop("Cannot load job without an ID")
      }
      
      if (!exists.Job(self$job_id)) {
        stop("Cannot find job")
      }
      
      con = openeo.server$getConnection()
      job_info = dbGetQuery(con, "select * from job where job_id = :id"
                            ,param = list(id=self$job_id))
      dbDisconnect(con)
      
      self$user_id = job_info$user_id
      self$status = job_info$status
      self$submitted = job_info$submitted
      self$last_update = job_info$last_update
      self$consumed_credits = job_info$consumed_credits
      
      if (is.null(job_info$output) || is.na(job_info$output) || length(job_info$output) < 1) {
        self$output = NULL
      } else {
        self$output = fromJSON(txt=decodeHex2Char(job_info$output),simplifyDataFrame = FALSE)
      }
      
      self$budget = job_info$budget
      self$title = job_info$title
      self$description = job_info$description
      self$plan = job_info$plan
      
      # when stored in a db then all the time the graph is loaded from db, regardless if it is published or not
      private$pg = ProcessGraph$new(graph_id = job_info$process_graph)
      
      self$process_graph = private$pg$buildExecutableProcessGraph(user = User$new()$load(user_id=self$user_id), job=self) #from db
      self$persistent = TRUE
      
      invisible(self)

    },

    shortInfo = function() {
      info = list(
        job_id = self$job_id,
        title = self$title,
        description = self$description,
        status = self$status,
        submitted = format(as_datetime(self$submitted),format="%Y-%m-%dT%H:%M:%SZ"),
        updated = format(as_datetime(self$last_update),format="%Y-%m-%dT%H:%M:%SZ"),
        plan = self$plan,
        costs = self$consumed_credits,
        budget = self$budget
      )
      
      return(info)
    },
    remove = function() {
      if (is.null(self$job_id) || !exists.Job(self$job_id)) {
        return(FALSE)
      }
      
      con = openeo.server$getConnection()
      success1 = dbExecute(con,"delete from process_graph 
                           where graph_id = (
                              select process_graph
                              from job
                              where job_id = :id)",param=list(id = self$job_id)) == 1
      success2 = dbExecute(con,"delete from job where job_id = :id",param=list(id = self$job_id)) == 1
      dbDisconnect(con)
      
      if (dir.exists(self$output.folder)) {
        unlink(self$output.folder,recursive = TRUE)
      }

      return(success1 && success2)
    },
    
    detailedInfo = function() {
      if (is.null(private$pg)) {
        stop("process_graph not loaded from db")
      }
      info = list(
        job_id = self$job_id,
        title = self$title,
        description = self$description,
        process_graph = private$pg$process_graph,
        output = self$output,
        status = self$status,
        submitted = format(as_datetime(self$submitted),format="%Y-%m-%dT%H:%M:%SZ"),
        updated = format(as_datetime(self$last_update),format="%Y-%m-%dT%H:%M:%SZ"),
        plan = self$plan,
        costs = self$consumed_credits,
        budget = self$budget
      )
      
      return(info)
    },
    
    getProcessGraph = function() {
      return(private$pg)
    },
    
    modifyProcessGraph = function(graph) {
      old_pg_id = private$pg$graph_id
      # graph is the list represenation of the process graph as parsed from the job input object
      private$pg = ProcessGraph$new(process_graph = graph, user_id = self$user_id)
      private$pg$graph_id = old_pg_id
      private$pg$update()
      
      user = User$new()$load(user_id=self$user_id)
      self$process_graph = private$pg$buildExecutableProcessGraph(user=user,job=self)
    },
    
    run = function() {

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
        self$results = NULL
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
    
  ),
  # active ----
  active = list(
    output.folder = function() {
      jobs.folder = "jobs"
      return(paste(openeo.server$workspaces.path, jobs.folder, self$job_id,sep="/"))
    }
  ), 
  # private ----
  private = list(
    # attributes ====
    pg = NULL,
    # functions ====
    newJobId = function() {
      randomString = createAlphaNumericId(n=1,length=15)
      
      
      if (exists.Job(randomString)) {
        # if id exists get a new one (recursive)
        return(private$newJobId())
      } else {
        return(randomString)
      }
    }
  )
)

# statics ====
is.Job = function(obj) {
  return("Job" %in% class(obj))
}

exists.Job = function(job_id) {
  if (nchar(job_id) == 15) {
    con = openeo.server$getConnection()
    result = dbGetQuery(con, "select count(*) from job where job_id = :id"
                        ,param = list(id=job_id)) == 1
    dbDisconnect(con)
    return(result)
  } else {
    return(FALSE)
  }
}

syncJobId = function() {
  randomString = paste("SYNC",createAlphaNumericId(n=1,length=11),sep="")
  
  
  if (exists.Job(randomString)) {
    # if id exists get a new one (recursive)
    return(syncJobId())
  } else {
    return(randomString)
  }
}