#' OpenEOServer
#' 
#' This is the server class, wich has different variables regarding the storage paths, as well as the loaded processes, products and
#' jobs.
#' 
#' @field api.version The current api version used
#' @field data.path The filesystem path where to find the datasets
#' @field workspaces.path The filesystem path where user data and jobs are stored
#' @field api.port The port where the plumber webservice is working under
#' @field processes This field is also managed during runtime. Here all template processes are listed
#' @field data A list of products offered by the service which is managed at runtime.
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
    # public ====
    public = list(
      # attributes ----
      api.version = "0.0.2",
      secret.key = NULL,
      
      data.path = NULL,
      workspaces.path = NULL,
      sqlite.path = NULL,
      
      api.port = NULL,
      host = NULL,
      mapserver.url = NULL, #assuming here a url, if not specified the backend is probably started with docker-compose
      
      processes = NULL,
      data = NULL,
      
      outputGDALFormats = NULL,
      outputOGRFormats = NULL,
      
      # functions ----
      initialize = function() {
        self$processes = list()
        self$data = list()
        
        drivers = gdalDrivers()
        ogr_drivers = ogrDrivers()
        self$outputGDALFormats = drivers[drivers$create,"name"]
        self$outputOGRFormats = ogr_drivers[ogr_drivers$write, "name"]
      },
      
      startup = function (port=8000,host="127.0.0.1",host_name="localhost") {
        if (! is.na(port)) {
          self$api.port = port
        }
        self$host = host_name
        
        batch_job_download_dir = paste(openeo.server$workspaces.path,"jobs",sep="/")
        
        if (! dir.exists(batch_job_download_dir)) {
          dir.create(batch_job_download_dir,recursive = TRUE)
        }
        
        self$initEnvironmentDefault()
        
        # migrate all user workspaces to /users/
        folder.names = list.files(openeo.server$workspaces.path,pattern = "[^openeo.sqlite|users|data|jobs|services]",full.names = TRUE)
        user_ids = list.files(openeo.server$workspaces.path,pattern = "[^openeo.sqlite|users|data|jobs|services]")
        if (length(user_ids) > 0) {
          if (!dir.exists(paste(openeo.server$workspaces.path,"users",sep="/"))) {
            dir.create(paste(openeo.server$workspaces.path,"users",sep="/"))
          }
          invisible(file.rename(from=folder.names,to=paste(openeo.server$workspaces.path,"users",user_ids,sep="/")))
        }
        
        root = createAPI()
        
        root$registerHook("exit", function(){
          print("Bye bye!")
        })
        
        job_downloads = PlumberStatic$new(batch_job_download_dir)
        root$mount("/api/result", job_downloads)
        
        root$run(port = self$api.port,host = host)
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
        con = self$getConnection()
        if (isJob(obj)) {
          # unlink(obj$filePath, recursive = TRUE,force=TRUE)
          dbExecute(con, "delete from job where job_id = :id",param=list(id=obj$job_id))
        } else if (isUser(obj)) {
          unlink(obj$workspace,recursive = TRUE)
          dbExecute(con, "delete from user where user_id = :id",param=list(id=obj$user_id))
        }
        dbDisconnect(con)
        
      },
      
      createJob = function(user,job_id = NULL, process_graph = process_graph, storeProcessGraph = TRUE) {
        if (is.null(job_id)) {
          job_id = private$newJobId()
        }
        if (!is.null(process_graph) && is.list(process_graph) && storeProcessGraph) {
          # get graph_id
          process_graph = self$createProcessGraph(process_graph, user$user_id)
        }
        
        job = Job$new(job_id = job_id, process_graph = process_graph,user_id = user$user_id)
        
        
        return(job)
      },

      createUser = function(user_name, password, silent=FALSE) {
        files.folder = "files"
        
        if (!self$userExists(user_name=user_name)) {
        id = private$newUserId()
        
        user = User$new(user_id = id)
        user$user_name = user_name
        user$password = password
        
        user_info = data.frame(user_id = id, user_name=user_name, password = password, login_secret = "")
        
        con = self$getConnection()
        if (dbGetQuery(con, "select count(*) from user where user_id=:id",
                       param=list(id = id)) == 0) {
          dbWriteTable(con,"user",as.data.frame(user_info),append=TRUE)
        }
        dbDisconnect(con)
        
        dir.create(user$workspace, showWarnings = FALSE)
        dir.create(paste(user$workspace, files.folder, sep="/"), showWarnings = FALSE)
        
          if (!silent) {
            return(user)
          }
        } else {
          user = self$loadUser(user_name=user_name)
          
          if (!silent) {
            return(user)
          }
        }
        
      },
      
      createProcessGraph = function(process_graph, user_id) {
        pgid = private$newProcessGraphId()
        
        if (is.list(process_graph)) {
          process_graph = toJSON(process_graph,auto_unbox=TRUE,pretty=TRUE)
        } else {
          stop("process_graph is no list object")
        }
        process_graph = encodeProcessGraph(process_graph)
        
        con = self$getConnection()
        dbExecute(con, "insert into process_graph (graph_id, user_id, process_graph) values (:graphId, :userId, :graph)",
                  param = list(graphId = pgid, userId = user_id, graph = process_graph))
        dbDisconnect(con)
        
        return(pgid)
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
        con = self$getConnection()
        if (!dbExistsTable(con,"user")) {
          dbExecute(con, "create table user (user_id integer, 
                    user_name text, 
                    password text, 
                    login_secret text)")
        }
        if (!dbExistsTable(con,"job")) {
          dbExecute(con, "create table job (job_id text, 
                    user_id integer, 
                    status text, 
                    submitted text,
                    last_update text,
                    consumed_credits integer,
                    process_graph text)")
        }
        if (!dbExistsTable(con,"process_graph")) {
          dbExecute(con, "create table process_graph (graph_id text, 
                    user_id integer, 
                    process_graph text)")
        }
        if (!dbExistsTable(con,"service")) {
          dbExecute(con, "create table service (
                    service_id text,
                    job_id text,
                    args text,
                    type text
          )")
        }
        
        dbDisconnect(con)
      },
      
      initEnvironmentDefault = function() {
        
        if (is.null(self$workspaces.path)) {
          self$workspaces.path <- getwd()
        }
        if (is.null(self$data.path)) {
          self$data.path <- paste(self$workspaces.path,"data",sep="/")
          
          if (!dir.exists(self$data.path)) {
            dir.create(self$data.path,recursive = TRUE)
          }
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
        
        if (is.null(self$mapserver.url)) {
          # in docker environment mapserver is accessible under
          # "mapserver"
          self$mapserver.url = "http://mapserver/cgi-bin/mapserv?"
        }
      },
      
      loadUser = function(user_id=NULL,user_name=NULL) {
        userExists = self$userExists(user_id=user_id, user_name = user_name)
        
        if (userExists) {
          
          con = self$getConnection()
          if (!is.null(user_id)) {
            
            user_info = dbGetQuery(con, "select * from user where user_id = :id"
                                   ,param = list(id=user_id))
            
            
          } else if (!is.null(user_name)){
            user_info = dbGetQuery(con, "select * from user where user_name = :name"
                                   ,param = list(name=user_name))
          } 
          dbDisconnect(con)  
        
          user = User$new(user_info$user_id)
          user$user_name = user_info$user_name
          return(user)
        } else {
          stop("Cannot load user. It doesn't exists or too many user entries.")
        }
        
      },
      loadJob = function(job_id) {
        if (self$jobExists(job_id)) {
          con = self$getConnection()
          job_info = dbGetQuery(con, "select * from job where job_id = :id"
                                 ,param = list(id=job_id))
          dbDisconnect(con)
          
          job = Job$new(job_id)
          job$user_id = job_info$user_id
          job$status = job_info$status
          job$submitted = job_info$submitted
          job$last_update = job_info$last_update
          job$consumed_credits = job_info$consumed_credits
          job$process_graph = self$loadProcessGraph(job_info$process_graph) #from db
          
          job$loadProcessGraph() # create executable graph and store output on job$output
          
          
          return(job)
        }
      },
      loadProcessGraph = function(graph_id) {
        # note: this is a function to load the process graph from db; NOT the one where the process graph is created
        if (self$graphExists(graph_id)) {
          con = self$getConnection()
          graph_binary = dbGetQuery(con, "select process_graph from process_graph where graph_id = :id",
                                    param = list(id = graph_id))[1,]
          dbDisconnect(con)
          graph_list = fromJSON(decodeProcessGraph(graph_binary),simplifyDataFrame = FALSE)
          return(graph_list)
        } else {
          return(NULL)
        }
        
      },
      jobExists = function(job_id) {
        if (nchar(job_id) == 15) {
          con = self$getConnection()
          result = dbGetQuery(con, "select count(*) from job where job_id = :id"
                              ,param = list(id=job_id)) == 1
          dbDisconnect(con)
          return(result)
        } else {
          return(FALSE)
        }
      },
      graphExists = function(graph_id) {
        if (nchar(graph_id) == 18) {
          con = self$getConnection()
          result = dbGetQuery(con, "select count(*) from process_graph where graph_id = :id"
                              ,param = list(id=graph_id)) == 1
          dbDisconnect(con)
          return(result)
        } else {
          return(FALSE)
        }
      },
      userExists = function(user_id = NULL, user_name=NULL) {
        if (is.null(user_id) && is.null(user_name)) {
          stop("Cannot search for user without any information.")
        }
        con = self$getConnection()
        if (is.null(user_id)) {
          #search by name
          exists = dbGetQuery(con,"select count(user_id) from user where user_name = :name",param=list(name=user_name)) == 1
        } else {
          #search by id
          exists = dbGetQuery(con,"select count(user_id) from user where user_id = :id",param=list(id = user_id)) == 1
        }
        dbDisconnect(con)
        return(exists)
      },
      deleteJob = function(job_id) {
        con = openeo.server$getConnection()
        success = dbExecute(con,"delete from job where job_id = :id",param=list(id = job_id)) == 1
        dbDisconnect(con)
        
        return(success)
      },
      runJob = function(job, outputPath,format=NULL) {
          job_id = job$job_id
          
          if (!dir.exists(outputPath)) {
            dir.create(outputPath,recursive = TRUE)
          }
          
          log = paste(outputPath, "process.log",sep="/")
          
          sink(file=log,append = TRUE,type = "output")
          tryCatch({
            
            # self$register(job)
            
            if ("output" %in% names(job) && "format" %in% names(job$output)) {
              format = job$output$format
            }
            
            if (is.null(format) || length(format)==0) {
              format = "GTiff" #TODO needs to be stated in server-class and also needs to be decided if gdal or ogr
            }
            
            con = openeo.server$getConnection()
            updateJobQuery = "update job set last_update = :time, status = :status where job_id = :job_id"
            dbExecute(con, updateJobQuery ,param=list(time=as.character(Sys.time()),
                                                      status="running",
                                                      job_id=job_id))
            dbDisconnect(con)
            
            cat("Start job processing...\n")
            result = job$run()
            cat("Job done\n")
            
            
            
            con = openeo.server$getConnection()
            updateJobQuery = "update job set last_update = :time, status = :status where job_id = :job_id"
            dbExecute(con, updateJobQuery ,param=list(time=as.character(Sys.time()),
                                                      status="finished",
                                                      job_id=job_id))
            dbDisconnect(con)
            cat("Set job to finished\n")
          
          
          
            cat("Creating output\n")
            openEO.R.Backend:::.create_output_no_response(result, format, dir = outputPath)
            cat("Output finished\n")
          }, error = function(e) {
            cat(str(e))
          }, finally={
            sink()
          })

      }
    ),
    # private ====
    private = list(
      loadDemoData = function() {
        if (! all(c("landsat7","sentinel2") %in% list.files(self$data.path))) {
          cat("Downloading the demo data...  ")

          data.path = gsub("/$","",self$data.path)
          
          dir.create(data.path, recursive = TRUE)
          zipfile = paste(data.path,"openeo-demo.zip",sep="/")
          download.file(url="https://uni-muenster.sciebo.de/s/aiAEcP6DtHQ7t2Q/download",
                        destfile = zipfile,
                        mode="wb",quiet = TRUE)
          cat("[done]\n")
          # unzip
          cat("Unzipping...  ")
          unzip(zipfile=zipfile, exdir = gsub("/$","",self$data.path))
          # remove zip
          file.remove(zipfile)
          
          cat("[done]\n")
        }
        
        self$data = list()
        
        loadLandsat7Dataset()
        loadSentinel2Data()
      },
      
      loadDemoProcesses = function() {
        self$processes = list()
        
        self$register(filter_daterange)
        self$register(find_min)
        self$register(calculate_ndvi)
        self$register(filter_bands)
        self$register(zonal_statistics)
        self$register(filter_bbox)

      },
      
      newJobId = function() {
        randomString = createAlphaNumericId(n=1,length=15)
        
        
        if (self$jobExists(randomString)) {
          # if id exists get a new one (recursive)
          return(private$newJobId())
        } else {
          return(randomString)
        }
      },
      
      newUserId = function() {
        id = runif(1, 10^8, (10^9-1))
        
        con = self$getConnection()
        userIdExists = dbGetQuery(con, "select count(*) from user where user_id = :id", param=list(id=id)) == 1
        dbDisconnect(con)
        
        if (userIdExists) {
          return(private$newUserId())
        } else {
          return(floor(id))
        }
      },
      
      newProcessGraphId = function() {
        randomString = createAlphaNumericId(n=1,length=18)
        
        con = self$getConnection()
        userIdExists = dbGetQuery(con, "select count(*) from process_graph where graph_id = :id", param=list(id=randomString)) == 1
        dbDisconnect(con)
        
        if (userIdExists) {
          return(private$newProcessGraphId())
        } else {
          return(randomString)
        }
      }
    )
)

# statics ====

createAlphaNumericId = function(n=1, length=15) {
  randomString <- c(1:n)                  
  for (i in 1:n) {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    length, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}


#' Creates a server instance
#' 
#' The function creates a new server instance on the global variable 'openeo.server'. The names for
#' this variable is reserved and should not be changed by any means. It will crash the system, since
#' many endpoints will be accessing and depending on the correctly set variable 'openeo.server'.
#' 
#' @export
createServerInstance = function() {
  assign("openeo.server", OpenEOServer$new(),envir=.GlobalEnv)
  invisible()
}

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