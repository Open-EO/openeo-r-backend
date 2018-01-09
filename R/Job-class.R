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
    
    initialize = function(job_id=NA,filePath=NA,process_graph=NA,user_id = NA) {
      if (is.na(job_id) || is.null(job_id)) {
        
        stop("Cannot create new Job. There is no job_id specified")
        # check if exists, repeate randomJobId until free
      } else {
        self$job_id = job_id
      }
      
      
      if (is.na(filePath) || is.null(filePath)) {
        self$filePath = paste(openeo$jobs.path,"/",self$job_id,sep="")
      } else {
        self$filePath = filePath
      }
      
      if (!is.na(user_id)) {
        self$user_id = user_id
      }
      
      
      self$process_graph = process_graph
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