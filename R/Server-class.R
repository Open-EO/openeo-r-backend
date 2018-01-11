#' OpenEOServer
#' 
#' This is the server class, wich has different variables regarding the storage paths, as well as the loaded processes, products and
#' jobs.
#' 
#' @field api.version The current api version used
#' @field data.path The filesystem path where to find the datasets
#' @field jobs.path The filesystem path where the jobs are stored
#' @field api.port The port where the plumber webservice is working under
#' @field jobs This will be managed during startup. Here all stored jobs under jobs.path are loaded into memory and stored here
#' @field processes This field is also managed during runtime. Here all template processes are listed
#' @field data A list of products offered by the service which is managed at runtime.
#' @field users The registered user on this server
#' 
#' @include processes.R
#' @include data.R
#' @importFrom plumber plumb
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom sodium sha256
#' @export
OpenEOServer <- R6Class(
    "OpenEOServer",
    public = list(
      api.version = NULL,
      secret.key = NULL,
      
      data.path = NULL,
      jobs.path = NULL,
      users.path = NULL,
      api.port = NULL,
      
      jobs = NULL,
      processes = NULL,
      data = NULL,
      users = NULL,
      
      initialize = function() {
        self$jobs = list()
        self$processes = list()
        self$data = list()
        self$users = list()
        
        private$initEnvironmentDefault()
      },
      
      startup = function (port=NA) {
        if (! is.na(port)) {
          self$api.port = port
        }
        
        # load descriptions, meta data and file links for provided data sets
        private$loadData()
        
        # register the processes provided by the server provider
        private$loadProcesses()
        
        # if there have been previous job postings load those jobs into the system
        private$loadExistingJobs()
        
        private$loadUsers()
        
        root = createAPI()
        
        root$registerHook("exit", function(){
          print("Bye bye!")
        })
        
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
          
        } else if (isUser(obj)) {
          listName = "users"
          
          newObj = list(obj)
          names(newObj) = c(obj$user_id)
        }else {
          warning("Cannot register object. It is neither Process, Product nor Job.")
          return()
        }
        
        self[[listName]] = append(self[[listName]],newObj)
        
      },
      
      deregister = function(obj) {
        
        if (isJob(obj)) {
          self$jobs[[paste(obj$job_id)]] <- NULL
        } else if (isUser(obj)) {
          self$users[[paste(obj$user_id)]] <- NULL
        }
        
      },
      
      delete = function(obj) {
        if (isJob(obj)) {
          unlink(obj$filePath, recursive = TRUE,force=TRUE)
          self$deregister(obj)
        } else if (isUser(obj)) {
          unlink(obj$workspace,recursive = TRUE)
          self$deregister(obj)
        }
        
      },
      
      createJob = function(job_id = NULL) {
        if (is.null(job_id)) {
          job_id = private$newJobId()
        }
        
        path=paste(openeo$jobs.path,"/",job_id,sep="")
        
        job = Job$new(job_id = job_id, filePath = path)
        
        return(job)
      },

      createUser = function(user_name, password) {
        id = private$newUserId()
        
        user = User$new(user_id = id)
        user$user_name = user_name
        user$password = password
        user$workspace = paste(self$users.path,id,sep="/")
        
        user$store()
        
        return(user)
        
      },
      
      getUserByName = function(user_name) {
        user_names = sapply(self$users, function(user) {
          return(user$user_name)
        })
        index = which(user_name %in% user_names)
        
        if (length(index) == 1) {
          return(openeo$users[[index]])
        } else {
          stop(paste("Cannot find user by user_name: ",user_names,sep=""))
          return()
        }
      }
      
      
    ),
    private = list(
      loadData = function() {
        self$data = list()
        
        loadLandsat7Dataset()
        loadSentinel2Data()
      },
      
      loadUser = function(id) {
        ids = list.files(self$users.path)
        if(! id %in% ids) {
          return()
        }
        
        workspace.path = paste(self$users.path, id,sep="/")
        parsedJson = fromJSON(paste(workspace.path,"user.json",sep="/"))
        user = User$new(id)
        user$user_name = parsedJson[["user_name"]]
        user$password = parsedJson[["password"]]
        user$jobs = parsedJson[["jobs"]]
        user$workspace = workspace.path
        
        self$register(user)
        
        return(user)
        
      },
      
      loadUsers = function() {
        self$users = list()
        
        for (user_id in list.files(self$users.path)) {
          private$loadUser(user_id)
        }
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
          parsedJson = fromJSON(paste(self$jobs.path,jobid,"process_graph.json",sep="/"))
          owner = parsedJson[["user_id"]]
          
          job = Job$new(job_id=jobid)
          job$user_id = owner
          job$submitted = parsedJson[["submitted"]]
          job$status = parsedJson[["status"]]
          job$loadProcessGraph()
          
          self$register(job)
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
      
      newUserId = function() {
        id = runif(1, 10^11, (10^12-1))
        if (id %in% list.files(self$users.path)) {
          return(self$newUserId())
        } else {
          return(floor(id))
        }
        
      },
      
      initEnvironmentDefault = function() {
        
        if (is.null(self$data.path)) {
          self$data.path <- paste(system.file(package="openEO.R.Backend"),"extdata",sep="/")
        }
        if (is.null(self$job.path)) {
          self$jobs.path <- "C:/code/openeo-files/jobs"
        }
        if (is.null(self$users.path)) {
          self$users.path <- "C:/code/openeo-files/users"
        }
        if (is.null(self$secret.key)) {
          self$secret.key <- sha256(charToRaw("openEO-R"))
        }
        
        if (is.null(self$api.port)) {
          self$api.port <- 8000
        }
      }
    )
)

#' @export
openeo = OpenEOServer$new()