UnboxedPlumber = R6Class(
  "UnboxedPlumber",
  inherit=plumber,
  public=list(getPrivate=function()private)
)


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
#' @include api.R
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
      api.version = "0.3.1",
      secret.key = NULL,
      
      data.path = NULL,
      workspaces.path = NULL,
      sqlite.path = NULL,
      
      udf_transactions.path = NULL,
      udf_cleanup = TRUE,
      
      api.port = NULL,
      host = NULL,
      baseserver.url = "http://localhost:8000/",
      mapserver.url = NULL, #assuming here a url, if not specified the backend is probably started with docker-compose
      oidcprovider.url = NULL,
      
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
        
        self$initEndpoints()
      },
      
      startup = function (port=8000,host="127.0.0.1",host_name="localhost") {
        if (! is.na(port)) {
          self$api.port = port
        }
        self$host = host_name
        
        # fill missing environment variables
        self$initEnvironmentDefault()
        
        # create folders if they don't exist already
        batch_job_download_dir = paste(openeo.server$workspaces.path,"jobs",sep="/")
        
        if (! dir.exists(batch_job_download_dir)) {
          dir.create(batch_job_download_dir,recursive = TRUE)
        }
        
        udf_temp_dir = paste(openeo.server$workspaces.path,"udf",sep="/")
        
        if (! dir.exists(udf_temp_dir)) {
          dir.create(udf_temp_dir,recursive = TRUE)
        }
        
        private$initRouter()
        createAPI()
        
        
        job_downloads = PlumberStatic$new(batch_job_download_dir)
        private$router$mount("/result", job_downloads)
        
        private$router$run(port = self$api.port,host = host)
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
          names(newObj) = c(obj$id)
          
        } else {
          warning("Cannot register object. It is neither Process, Product nor Job.")
          return()
        }
        
        self[[listName]] = append(self[[listName]],newObj)
        
      },

      createUser = function(user_name, password, budget=-1, storage_quota = 200000000, silent=FALSE) {
        user = User$new()
        user$user_name = user_name
        user$password = password
        user$budget = budget
        user$storage_quota = storage_quota
        
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
                    login_secret text,
                    budget real,
                    storage_quota integer)")
        } else {
          columns = colnames(con %>% dbGetQuery("select * from user limit 0"))
          
          if (!"budget" %in% columns) {
            addColumnsQuery = "alter table user add budget real"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (! "storage_quota" %in% columns) {
            addColumnsQuery = "alter table user add storage_quota integer"
            con %>% dbExecute(addColumnsQuery)
          }
        }
        
        if (!dbExistsTable(con,"job")) {
          dbExecute(con, "create table job (job_id text, 
                    user_id integer, 
                    status text, 
                    submitted text,
                    last_update text,
                    consumed_credits integer,
                    output text,
                    budget real,
                    title text,
                    description text,
                    plan text,
                    process_graph text)")
        } else {
          # migration
          columns = colnames(con %>% dbGetQuery("select * from job limit 0"))
          
          if (!"budget" %in% columns) {
            addColumnsQuery = "alter table job add budget real"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (! "output" %in% columns) {
            addColumnsQuery = "alter table job add output text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (! "title" %in% columns) {
            addColumnsQuery = "alter table job add title text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (! "description" %in% columns) {
            addColumnsQuery = "alter table job add description text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (! "plan" %in% columns) {
            addColumnsQuery = "alter table job add plan text"
            con %>% dbExecute(addColumnsQuery)
          }
        }
        
        
        if (!dbExistsTable(con,"process_graph")) {
          dbExecute(con, "create table process_graph (graph_id text, 
                    user_id integer,
                    title text,
                    description text,
                    process_graph text)")
        } else {
          columns = colnames(con %>% dbGetQuery("select * from process_graph limit 0"))
          
          if (!"title" %in% columns) {
            addColumnsQuery = "alter table process_graph add title text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (! "description" %in% columns) {
            addColumnsQuery = "alter table process_graph add description text"
            con %>% dbExecute(addColumnsQuery)
          }
        }
        
        if (!dbExistsTable(con,"service")) {
          dbExecute(con, "create table service (
                    service_id text,
                    job_id text,
                    title text,
                    description text,
                    type text,
                    parameters text,
                    attributes text,
                    plan text,
                    costs real,
                    budget real,
                    enabled integer,
                    submitted datetime
          )")
        } else {
          columns = colnames(con %>% dbGetQuery("select * from service limit 0"))
          #args -> parameters
          if (!"parameters" %in% columns) {
            addColumnsQuery = "alter table service add parameters text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (!"title" %in% columns) {
            addColumnsQuery = "alter table service add title text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (!"description" %in% columns) {
            addColumnsQuery = "alter table service add description text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (!"type" %in% columns) {
            addColumnsQuery = "alter table service add type text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (!"attributes" %in% columns) {
            addColumnsQuery = "alter table service add attributes text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (!"plan" %in% columns) {
            addColumnsQuery = "alter table service add plan text"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (!"costs" %in% columns) {
            addColumnsQuery = "alter table service add costs real"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (!"budget" %in% columns) {
            addColumnsQuery = "alter table service add budget real"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (!"enabled" %in% columns) {
            addColumnsQuery = "alter table service add enabled integer"
            con %>% dbExecute(addColumnsQuery)
          }
          
          if (!"submitted" %in% columns) {
            addColumnsQuery = "alter table service add submitted datetime"
            con %>% dbExecute(addColumnsQuery)
          }
        }
        
        if (!dbExistsTable(con,"udf")) {
          dbExecute(con, "create table udf (
                    udf_id text,
                    job_id text,
                    start_date datetime default current_timestamp,
                    end_date datetime,
                    status text
          )")
        }
        
        if (!dbExistsTable(con,"job_result")) {
          dbExecute(con, "create table job_result (
                    job_id text,
                    path text,
                    created datetime,
                    expiry datetime,
                    size long
          )")
        }
        
        if (!dbExistsTable(con,"log")) {
          dbExecute(con, "create table log (
                    timestamp datetime,
                    job_id text,
                    service_id text,
                    process_id text,
                    category text,
                    message text
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
        }
        if (!dir.exists(self$data.path)) {
          dir.create(self$data.path,recursive = TRUE)
        }
        
        if (is.null(self$udf_transactions.path)) {
          self$udf_transactions.path = paste(self$workspaces.path,"udf",sep="/")
        }
        if (!dir.exists(self$udf_transactions.path)) {
          dir.create(self$udf_transactions.path, recursive = TRUE)
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

      runJob = function(job, format=NULL, response=FALSE, res = NULL) {
          logger = Logger$new(process=list(process_id = "job_runner"), job = job)
          
          job_id = job$job_id
          
          #  && !dir.exists(job$output.folder)
          if (!response) {
            if (!dir.exists(job$output.folder)) {
              dir.create(job$output.folder,recursive = TRUE)
            }
            
            log = paste(job$output.folder, "process.log",sep="/")
            
            logToFile(file=log)
          }
          
          
          tryCatch({

            if ("output" %in% names(job) && "format" %in% names(job$output)) {
              format = job$output$format
            }
            
            if (is.null(format) || length(format)==0) {
              format = "GTiff" #TODO needs to be stated in server-class and also needs to be decided if gdal or ogr
            }
            
            job = job$run(logger = logger)


            if (job$status == "error") {
              logger$error("Canceling output creation due to prior error")
            }

            if (!response) {
              logger$info("Creating output without HTTP response")
              openEO.R.Backend:::.create_output_no_response(job$results, format, dir = job$output.folder, logger = logger)
            } else {
              logger$info("Creating output and HTTP response")
              
              if (is.null(res)) {
                logger$error("Passed no response object. Please provide parameter 'res' from plumber")
              }
              
              return(.create_output(res = res,result = job$results, format = format, logger = logger))
            }
                      
            logger$info("Output finished")
          }, error = function(e) {
            cat(str(e))
          }, finally={
            removeJobsUdfData(job)
            
            if (!response) {
              logToConsole()
            }
          })

      },
      initEndpoints = function() {
        private$endpoints = tibble(path=character(0), method = character(0))
      },
      registerEndpoint = function(path, method, filters=list(),handler=NULL, serializer=serializer_unboxed_json(),
                                  withCORS=TRUE, unsupported = FALSE) {
        if (!unsupported) {
          private$endpoints = private$endpoints %>% add_row(path=path,method=method)
        } else {
          if (is.null(handler)) {
            handler = .not_implemented_yet
          }
        }
        
        
        if (!is.null(private$router)) {
          plumber.path = path %>% gsub(pattern="\\{",replacement="<") %>% gsub(pattern="\\}",replacement=">")
          endpoint=private$router$createEndpoint(methods = method,path = plumber.path,handler = handler,serializer = serializer)
          endpoint$registerFilter(filter = filters)
          
          private$router$handle(endpoint = endpoint)
          
          if (withCORS) {
            corsEndpoint = private$router$createEndpoint(methods = "OPTIONS",path = plumber.path,handler = .cors_option_bypass)
            #TODO register Filters?
            private$router$handle(endpoint = corsEndpoint)
          }
        }
        
        
        invisible(self)
      },
      
      getEndpoints = function() {
        return(private$endpoints)
      },
      
      createFilter = function(name, expr, serializer) {
        if (is.null(private$router)) return(NULL)
        
        return(private$router$createFilter(name, expr, serializer))
      }
    ),
    # private ----
    private = list(
      # attributes ====
      endpoints = NULL,
      router = NULL,
      
      # functions ====
      loadDemoData = function() {
        landsat7_md_url = "https://uni-muenster.sciebo.de/s/D2HMuKlxo2dxeWb/download"
        sentinel2_md_url = "https://uni-muenster.sciebo.de/s/rqfiErEIV0wALjp/download"
        
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
        
        # check metadata files and download them if missing
        ls7path = paste(self$data.path,"landsat7",sep="/")
        ls7md = paste(ls7path,"md.json",sep="/")
        ls7lookup = paste(ls7path,"lookup.csv",sep="/")
        if (! file.exists(ls7md)) {
          cat("Downloading missing metadata file for Landsat 7 dataset... ")
          download.file(url = paste(landsat7_md_url,"?files=md.json",sep=""),
                        destfile = ls7md,
                        quiet=TRUE)
          cat("[done]\n")
        }
        if (! file.exists(ls7lookup)) {
          cat("Downloading missing lookup table for Landsat 7 dataset... ")
          download.file(url = paste(landsat7_md_url,"?files=lookup.csv",sep=""),
                        destfile = ls7lookup,
                        quiet=TRUE)
          cat("[done]\n")
        }
        
        s2path = paste(self$data.path,"sentinel2",sep="/")
        s2md = paste(s2path,"md.json",sep="/")
        s2lookup = paste(s2path,"lookup.csv",sep="/")
        if (! file.exists(s2md)) {
          cat("Downloading missing metadata file for Sentinel 2 dataset... ")
          download.file(url = paste(sentinel2_md_url,"?files=md.json",sep=""),
                        destfile = s2md,
                        quiet=TRUE)
          cat("[done]\n")
        }
        if (! file.exists(s2lookup)) {
          cat("Downloading missing lookup table for Sentinel 2 dataset... ")
          download.file(url = paste(sentinel2_md_url,"?files=lookup.csv",sep=""),
                        destfile = s2lookup,
                        quiet=TRUE)
          cat("[done]\n")
        }
        
        self$data = list()
        cat("Loading demo data sets...")
        # loadLandsat7Dataset()
        # loadSentinel2Data()
        
        importCollection(paste(self$data.path,"sentinel2",sep="/"))$addSelfReferenceLink() %>% openeo.server$register()
        importCollection(paste(self$data.path,"landsat7",sep="/"),fun=raster)$addSelfReferenceLink() %>% openeo.server$register()
        # 1 banded granules have to use 
        # raster function, multiband = brick
        
        #TODO set self reference link for detailed description
        
        cat("[done]\n")
      },
      
      initRouter = function() {
        if(!is.null(private$endpoints) && nrow(private$endpoints)>=1) self$initEndpoints()
        
        private$router=OpenEORouter$new()
        
        # add the CORS filter to the router
        private$router$registerHook("postroute",.cors_filter)
        
        private$router$registerHook("exit", function(){
          print("Bye bye!")
        })
      },
      
      loadDemoProcesses = function() {
        self$processes = list()
        
        self$register(filter_daterange)
        self$register(find_min)
        self$register(calculate_ndvi)
        self$register(filter_bands)
        self$register(zonal_statistics)
        self$register(filter_bbox)
        self$register(aggregate_time)
        self$register(apply_pixel)
        self$register(get_collection)
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
  invisible(openeo.server)
}


# DatabaseEntity Interface ----
#' Interface for all elements stored in a database
#' 
#' The class does not offer much functionality, but reserves some function names to be available to all
#' inheriting classes. Usually the ineriting class should implement those functions in a proper way.
DatabaseEntity = R6Class(
  "DatabaseEntity",
  public = list(
    load = function() {
      
    },
    store = function() {
      
    },
    remove = function() {
      
    }
  )
)


openeo.globals = plumber:::.globals
