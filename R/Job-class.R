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
        # check if exists, repete randomJobId until free
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
      self$process_graph = loadProcess(parsedJson[["process_graph"]])
    },
    
    loadProcess = function(parsedJson) {
      processId = parsedJson[["process_id"]]
      #TODO: add cases for udfs
      if (!is.null(processId) && processId %in% names(openeo$processes)) {
        process = openeo$processes[[processId]]
        return(process$as.executable(parsedJson),self)
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
    }
    
  )
)