#' OpenEOServer
#' 
#' This is the server class, wich has different variables regarding the storage paths, as well as the loaded processes, products and
#' jobs.
#' 
#' @field api.version The current api version used
#' @field project.path The filesystem path where to find the plumber api file
#' @field data.path The filesystem path where to find the datasets
#' @field jobs.path The filesystem path where the jobs are stored
#' @field api.port The port where the plumber webservice is working under
#' @field jobs This will be managed during startup. Here all stored jobs under jobs.path are loaded into memory and stored here
#' @field processes This field is also managed during runtime. Here all template processes are listed
#' @field data A list of products offered by the service which is managed at runtime.
#' 
#' @include processes.R
#' @include data.R
#' @importFrom plumber plumb
#' @importFrom R6 R6Class
#' @export
OpenEOServer <- R6Class(
    "OpenEOServer",
    public = list(
      api.version = NULL,
      project.path = NULL,
      data.path = NULL,
      jobs.path = NULL,
      api.port = NULL,
      api.path = NULL,
      
      jobs = NULL,
      processes = NULL,
      data = NULL,
      
      initialize = function() {
        self$jobs = list()
        self$processes = list()
        self$data = list()
      },
      
      startup = function (port=NA) {
        if (! is.na(port)) {
          self$api.port = port
        }
        
        private$initEnvironmentDefault()
        
        # load descriptions, meta data and file links for provided data sets
        private$loadData()
        
        # register the processes provided by the server provider
        private$loadProcesses()
        
        # if there have been previous job postings load those jobs into the system
        private$loadExistingJobs()
        
        setwd(self$project.path)
        
        root <- plumb(self$api.path)
        
        root$run(port = self$api.port)
      },
      
      register = function(obj) {
        listName = NULL
        newObj = NULL
        
        if (isProcess(obj)) {
          if (is.null(self$processes)) {
            self$processes = list()
          }
          listName = "processes"
          
          newObj = list(obj)
          names(newObj) = obj$process_id
          
        } else if (isProduct(obj)) {
          if (is.null(self$data)) {
            self$data = list()
          }
          listName = "data"
          
          newObj = list(obj)
          names(newObj) = c(obj$product_id)
          
        } else if (isJob(obj)) {
          if (is.null(self$jobs)) {
            self$jobs = list()
          }
          listName = "jobs"
          
          newObj = list(obj)
          names(newObj) = c(obj$job_id)
          
        } else {
          warning("Cannot register object. It is neither Process, Product nor Job.")
          return()
        }
        
        self[[listName]] = append(self[[listName]],newObj)
        
      },
      
      deregister = function(obj) {
        
        if (isJob(obj)) {
          self$jobs[[obj$job_id]] <- NULL
        }
        
      },
      
      delete = function(obj) {
        if (isJob(obj)) {
          unlink(obj$filePath, recursive = TRUE,force=TRUE)
          self$deregister(obj)
        }
        
      },
      
      newJobId = function(n=1, length=15) {
        # cudos to https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
        randomString <- c(1:n)                  
        for (i in 1:n) {
          randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                          length, replace=TRUE),
                                   collapse="")
        }
        
        if (randomString %in% list.files(self$jobs.path)) {
          # if id exists get a new one (recursive)
          return(self$newJobId())
        } else {
          return(randomString)
        }
      },
      
      storeJob = function(job,json=NA) {
        dir.create(job$filePath)
        write(x=json,file=paste(job$filePath,"/process_graph.json",sep=""))
      }
    ),
    private = list(
      loadData = function() {
        self$data = list()
        
        loadLandsat7Dataset()
        loadSentinel2Data()
      },
      
      loadProcesses = function() {
        self$processes = list()
        
        self$register(filter_daterange)
        self$register(find_min)
        self$register(calculate_ndvi)
        
        #filter_sp_extent = Process$new()
        #filter_sp_extent$register()
        
        #crop_extent = Process$new()
        #crop_extent$register()

      },
      
      loadExistingJobs = function() {
        self$jobs = list()
        
        for (jobid in list.files(self$jobs.path)) {
          job = Job$new(job_id=jobid)
          
          self$register(job)
        }
      },
      
      initEnvironmentDefault = function() {
        
        if (is.null(self$api.path)) {
          self$api.path <- "C:/code/openeo.r.backend/R/api.R"
        }
        
        if (is.null(self$project.path)) {
          self$project.path <- "C:/code/openeo.r.backend"
        }
        if (is.null(self$data.path)) {
          self$data.path <- "C:/code/openeo.r.backend/data"
        }
        if (is.null(self$job.path)) {
          self$jobs.path <- "C:/code/openeo-files/jobs"
        }
        if (is.null(self$api.port)) {
          self$api.port <- 8000
        }
      }
    )
)

#' @export
openeo = OpenEOServer$new()