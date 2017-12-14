#' @include Process-class.R
#' @importFrom R6 R6Class
#' @export
Job <- R6Class(
  "Job",
  public = list(
    job_id = NULL,
    filePath = NULL,
    process = NULL,
    
    initialize = function(job_id=NA,filePath=NA,process=NA) {
      if (is.na(job_id) || is.null(job_id)) {
        self$job_id = self$randomJobId()
        # check if exists, repete randomJobId until free
      } else {
        self$job_id = job_id
      }
      
      
      if (is.na(filePath) || is.null(filePath)) {
        self$filePath = paste(openeo$jobs.path,"/",self$job_id,"/",sep="")
      } else {
        self$filePath = filePath
      }
      
      self$process = process
    },
    
    store = function(json=NA) {
      dir.create(self$filePath)
      write(x=json,file=paste(self$filePath,"job.json",sep=""))
    },
    
    randomJobId = function(n=1, length=15) {
      # cudos to https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
      randomString <- c(1:n)                  
      for (i in 1:n) {
        randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                 length, replace=TRUE),
                                 collapse="")
      }
      return(randomString)
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
      
    }
    
  )
)