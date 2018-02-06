#' OpenEOServer
#' 
#' This is the server class, wich has different variables regarding the storage paths, as well as the loaded processes, products and
#' jobs.
#' 
#' @field api.version The current api version used
#' @field data.path The filesystem path where to find the datasets
#' @field workspaces.path The filesystem path where user data and jobs are stored
#' @field api.port The port where the plumber webservice is working under
#' @field jobs This will be managed during startup. Here all the users submitted jobs are registered
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
#' @import DBI
#' @export
OpenEOServer <- R6Class(
    "OpenEOServer",
    public = list(
      api.version = NULL,
      secret.key = NULL,
      
      data.path = NULL,
      workspaces.path = NULL,
      sqlite.path = NULL,
      
      api.port = NULL,
      
      processes = NULL,
      data = NULL,
      
      database = NULL,
      
      initialize = function() {
        self$processes = list()
        self$data = list()
      },
      
      startup = function (port=NA) {
        if (! is.na(port)) {
          self$api.port = port
        }
        
        self$initEnvironmentDefault()
        
        
        # load descriptions, meta data and file links for provided data sets
        # private$loadData()
        
        # register the processes provided by the server provider
        # private$loadProcesses()
        
        # private$loadUsers()
        
        # if there have been previous job postings load those jobs into the system
        # private$loadExistingJobs()
        
        root = createAPI()
        
        root$registerHook("exit", function(){
          dbDisconnect(self$database)
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
          
        } else {
          warning("Cannot register object. It is neither Process, Product nor Job.")
          return()
        }
        
        self[[listName]] = append(self[[listName]],newObj)
        
      },
      
      delete = function(obj) {
        if (isJob(obj)) {
          # unlink(obj$filePath, recursive = TRUE,force=TRUE)
          dbExecute(openeo.server$database, "delete from job where job_id = :id",param=list(id=obj$job_id))
        } else if (isUser(obj)) {
          unlink(obj$workspace,recursive = TRUE)
          dbExecute(openeo.server$database, "delete from user where user_id = :id",param=list(id=obj$user_id))
        }
        
      },
      
      createJob = function(user,job_id = NULL, process_graph = process_graph) {
        if (is.null(job_id)) {
          job_id = private$newJobId()
        }
        
        job = Job$new(job_id = job_id, process_graph = process_graph,user_id = user$user_id)
        
        return(job)
      },

      createUser = function(user_name, password, silent=FALSE) {
        files.folder = "files"
        id = private$newUserId()
        
        user = User$new(user_id = id)
        user$user_name = user_name
        user$password = password
        
        user_info = data.frame(user_id = id, user_name=user_name, password = password, login_secret = "")
        if (dbGetQuery(self$database, "select count(*) from user where user_id=:id",
                       param=list(id = id)) == 0) {
          dbWriteTable(self$database,"user",as.data.frame(user_info),append=TRUE)
        }
        
        dir.create(user$workspace, showWarnings = FALSE)
        dir.create(paste(user$workspace,files.folder,sep="/"), showWarnings = FALSE)
        
        if (!silent) {
          return(user)
        }
        
      },

      
      loadDemo = function() {
        self$initEnvironmentDefault()
        self$initializeDatabase()
        
        private$loadDemoData()
        private$loadDemoProcesses()
      }, 
      
      getConnection = function() {
        return(dbConnect(RSQLite::SQLite(),self$sqlite.path))
      },
      
      initializeDatabase = function() {
        self$database = self$getConnection()
        
        if (!dbExistsTable(self$database,"user")) {
          dbExecute(self$database, "create table user (user_id integer, 
                    user_name text, 
                    password text, 
                    login_secret text)")
        }
        if (!dbExistsTable(self$database,"job")) {
          dbExecute(self$database, "create table job (job_id text, 
                    user_id integer, 
                    status text, 
                    submitted text,
                    last_update text,
                    consumed_credits integer,
                    process_graph text)")
        }
      },
      
      initEnvironmentDefault = function() {
        
        if (is.null(self$data.path)) {
          self$data.path <- paste(system.file(package="openEO.R.Backend"),"extdata",sep="/")
        }
        if (is.null(self$workspaces.path)) {
          self$workspaces.path <- getwd()
        }
        if (is.null(self$secret.key)) {
          self$secret.key <- sha256(charToRaw("openEO-R"))
        }
        if (is.null(self$sqlite.path)) {
          self$sqlite.path <- paste(self$workspaces.path,"openeo.sqlite",sep="/")
        }
        
        if (is.null(self$api.port)) {
          self$api.port <- 8000
        }
      },
      
      loadUser = function(user_id) {
        
        if (dbGetQuery(openeo.server$database, "select count(*) from user where user_id = :id"
                       ,param = list(id=user_id)) == 1
        ) {
          user_info = dbGetQuery(openeo.server$database, "select * from user where user_id = :id"
                                 ,param = list(id=user_id))
          
          user = User$new(user_id)
          user$user_name = user_info$user_name
          return(user)
        } else {
          stop("Cannot load user. It doesn't exists or too many user entries.")
        }
        
      },
      loadJob = function(job_id) {
        if (self$jobExists(job_id)) {
          job_info = dbGetQuery(self$database, "select * from job where job_id = :id"
                                 ,param = list(id=job_id))
          job = Job$new(job_id)
          job$user_id = job_info$user_id
          job$status = job_info$status
          job$submitted = job_info$submitted
          job$last_update = job_info$last_update
          job$consumed_credits = job_info$consumed_credits
          job$process_graph = fromJSON(decodeProcessGraph(job_info$process_graph),simplifyDataFrame = FALSE)
          
          job$loadProcessGraph()
          
          
          return(job)
        }
      },
      jobExists = function(job_id) {
        return(dbGetQuery(openeo.server$database, "select count(*) from job where job_id = :id"
                  ,param = list(id=job_id)) == 1)
      }
    ),
    private = list(
      loadDemoData = function() {
        self$data = list()
        
        loadLandsat7Dataset()
        loadSentinel2Data()
      },
      
      loadDemoProcesses = function() {
        self$processes = list()
        
        self$register(filter_daterange)
        self$register(find_min)
        self$register(calculate_ndvi)
        
        #filter_sp_extent = Process$new()
        #filter_sp_extent$register()
        
        #crop_extent = Process$new()
        #crop_extent$register()

      },
      
      newJobId = function(n=1, length=15) {
        # cudos to https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
        randomString <- c(1:n)                  
        for (i in 1:n) {
          randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                          length, replace=TRUE),
                                   collapse="")
        }
        
        jobIdExists = dbGetQuery(self$database, "select count(*) from job where job_id = :id", param=list(id=randomString)) == 1
        
        if (jobIdExists) {
          # if id exists get a new one (recursive)
          return(self$newJobId())
        } else {
          return(randomString)
        }
      },
      
      newUserId = function() {
        id = runif(1, 10^11, (10^12-1))
        if (id %in% list.files(self$workspaces.path)) {
          return(self$newUserId())
        } else {
          return(floor(id))
        }
        
      }
    )
)

#' @export
openeo.server = OpenEOServer$new()

#' Migration tool
#' 
#' This functions migrates the old file based information storage into the sqlite solution.
#' 
#' @param db sqlite database connection
#' @param workspace the user workspace, where to find the user folders
#' 
#' @export
migrateFilesToDB = function(db, workspace) {
  df = data.frame(user_id=list.files(workspace),workspace=list.files(workspace,full.names = TRUE))
  df$jobs_workspace = paste(df$workspace,"jobs",sep="/")
  df$files_workspace = paste(df$workspace,"files",sep="/")
  df$user_file = paste(df$workspace, "user.json",sep="/")
  
  for (row_no in 1:nrow(df)) {
    row = df[row_no,]
    # read user.json
    if (file.exists(row$user_file)) {
      user_info = fromJSON(row$user_file)
      user_info$login_secret = ""
      if (dbGetQuery(db,"select count(*) from user where user_id=:id",param=list(id=user_info$user_id))==0) {
        dbWriteTable(db,"user",as.data.frame(user_info),append=TRUE)
      }
    }
    
    # make job list
    if (dir.exists(row$jobs_workspace)) {
      jobs = data.frame(job_id = paste(list.files(row$jobs_workspace)), 
                        job_path = paste(list.files(row$jobs_workspace, full.names=TRUE),"process_graph.json",sep="/"),
                        stringsAsFactors=FALSE)
      for(job_row_no in 1:nrow(jobs)) {
        job_row = jobs[job_row_no,]
        
        if (file.exists(job_row$job_path)) {
          
          if (dbExistsTable(db,"job") && 
              dbGetQuery(db, "select count(*) from job where job_id = :id",param=list(id=job_row$job_id)) == 0) {
            job = fromJSON(paste(job_row$job_path))
            job$job_id = job_row$job_id
            process_graph = job$process_graph
            job$submitted = job$submitted
            job$process_graph = as.character(toJSON(process_graph,auto_unbox = TRUE))
            
            
            columns = dbListFields(db,"job")
            dbWriteTable(db,"job",as.data.frame(job)[,columns],append=TRUE)
          }  
        }
      }
    }
  }
}