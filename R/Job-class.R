#' Job
#' 
#' This class represents an openEO job. Which is submitted by an user, containing an executable process graph.
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
#' @export
Job <- R6Class(
  "Job",
  public = list(
    job_id = NULL,
    status=NULL,
    process_graph = NULL,
    view=NULL,
    submitted=NULL,
    last_update=NULL,
    user_id=NULL,
    consumed_credits=NULL,
    
    filePath = NULL,
    
    initialize = function(job_id=NA,filePath=NA,process_graph=NA) {
      if (is.na(job_id) || is.null(job_id)) {
        self$job_id = self$randomJobId()
        # check if exists, repeate randomJobId until free
      } else {
        self$job_id = job_id
      }
      
      
      if (is.na(filePath) || is.null(filePath)) {
        self$filePath = paste(openeo$jobs.path,"/",self$job_id,sep="")
      } else {
        self$filePath = filePath
      }
      
      
      self$process_graph = process_graph
    },
    
    store = function(json=NA) {
      dir.create(self$filePath)
      write(x=json,file=paste(self$filePath,"/process_graph.json",sep=""))
    },
    
    loadProcessGraph = function() {
      parsedJson = read_json(paste(self$filePath,"/process_graph.json",sep=""))
      self$process_graph = self$loadProcess(parsedJson[["process_graph"]])
      invisible(self)
    },
    
    loadProcess = function(parsedJson) {
      processId = parsedJson[["process_id"]]
      #TODO: add cases for udfs
      if (!is.null(processId) && processId %in% names(openeo$processes)) {
        process = openeo$processes[[processId]]
        return(process$as.executable(parsedJson,self))
      } else {
        stop(paste("Cannot load process",processId))
      }
    },
    
    randomJobId = function(n=1, length=15) {
      # cudos to https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
      randomString <- c(1:n)                  
      for (i in 1:n) {
        randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                        length, replace=TRUE),
                                 collapse="")
      }
      
      if (randomString %in% list.files(openeo$jobs.path)) {
        # if id exists get a new one (recursive)
        return(self$randomJobId())
      } else {
        return(randomString)
      }
    },
    
    register = function() {
      if (is.null(openeo$jobs)) {
        openeo$jobs = list()
      }
      newJob = list(self)
      names(newJob) = c(self$job_id)
      
      openeo$jobs = append(openeo$jobs,newJob)
    },
    
    deregister = function() {
      openeo$jobs[[self$job_id]] <- NULL
    },
    
    delete = function() {
      unlink(self$filePath, recursive = TRUE,force=TRUE)
      self$deregister()
    },
    
    run = function() {
      if (is.na(self$process_graph) || is.null(self$process_graph) || class(self$process_graph) == "list") {
          self$loadProcessGraph()
      }
      self$process_graph$execute()
    }
    
  )
)