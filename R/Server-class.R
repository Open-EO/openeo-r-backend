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
        
        if (is.Process(obj)) {
          if (is.null(self$processes)) {
            self$processes = list()
          }
          listName = "processes"
          
          newObj = list(obj)
          names(newObj) = obj$process_id
          
        } else if (is.Product(obj)) {
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

      createUser = function(user_name, password, silent=FALSE) {
        user = User$new()
        user$user_name = user_name
        user$password = password
        
        user$store()
        
        if (silent) {
          invisible(user)
        } else {
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

      runJob = function(job, format=NULL) {
          job_id = job$job_id
          
          if (!dir.exists(job$output.folder)) {
            dir.create(job$output.folder,recursive = TRUE)
          }
          
          log = paste(job$output.folder, "process.log",sep="/")
          
          logToFile(file=log)
          tryCatch({

            if ("output" %in% names(job) && "format" %in% names(job$output)) {
              format = job$output$format
            }
            
            if (is.null(format) || length(format)==0) {
              format = "GTiff" #TODO needs to be stated in server-class and also needs to be decided if gdal or ogr
            }
            
            job = job$run()
            

            if (job$status == "error") {
              stop("Canceling output creation due to prior error")
            }

            cat("Creating output\n")
            openEO.R.Backend:::.create_output_no_response(job$results, format, dir = job$output.folder)
            cat("Output finished\n")
          }, error = function(e) {
            cat(str(e))
          }, finally={
            logToConsole()
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
          download.file(url="https://uni-muenster.sciebo.de/s/lwtfzvRjsI0oUiB/download",
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

      }
      

      
      
    )
)

# logging ====
logToConsole = function() {
  sink()
}
logToNull = function() {
  if (tolower(Sys.info()["sysname"]) == "windows") {
    sink("nul")
  } else {
    sink("/dev/null")
  }
}
logToFile = function(file) {
  sink(file = file,append = TRUE,type="output")
}

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
