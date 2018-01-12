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
    
    initialize = function(job_id=NULL,filePath=NULL,process_graph=NULL,user_id = NULL) {
      if (is.null(job_id)||missing(job_id)) {
        stop("Cannot create new Job. There is no job_id specified")
      } else {
        self$job_id = job_id
      }
      
      
      if (is.null(filePath) || missing(filePath)) {
        stop("Cannot initialize Job without the job workspace")
      } else {
        self$filePath = filePath
      }
      
      if (!is.null(user_id)) {
        self$user_id = user_id
      }
      
      
      self$process_graph = process_graph
    },
    
    store = function(json=NA) {
      dir.create(self$filePath)
      write(x=json,file=paste(self$filePath,"/process_graph.json",sep=""))
    },
    
    loadProcessGraph = function() {
      parsedJson = fromJSON(paste(self$filePath,"/process_graph.json",sep=""))
      self$process_graph = self$loadProcess(parsedJson[["process_graph"]])
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
        submitted = self$submitted,
        last_update = self$last_update,
        consumed_credits = self$consumed_credits
      )
    },
    
    run = function() {
      if (!isProcess(self$process_graph)) {
          self$loadProcessGraph()
      }
      self$process_graph$execute()
    }
    
  )
)

isJob = function(obj) {
  return("Job" %in% class(obj))
}