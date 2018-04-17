#' @docType class
#' @importFrom R6 R6Class
#' @export
User <- R6Class(
  "User",
  public = list(
    # public ----
    # attributes ====
    user_id = NULL,
    user_name = NULL,
    password = NULL,
    token = NULL,
    
    # functions ====
    initialize = function(user_id = NULL) {
      self$user_id = user_id
    },
    
    toList = function() {
      return(
        list(
          user_id = self$user_id,
          user_name = self$user_name,
          password = self$password,
          jobs = self$jobs
        )
      )
    },
    
    fileList = function() {
      files.df = self$files
      rownames(files.df) <- NULL
      
      return(apply(files.df,1,function(row) {
       return(list(
         name = row["link"],
         size = row["size"]
       )) 
      }))
    },
    
    load = function(user_id=NULL,user_name=NULL) {
      userExists = exists.User(user_id=user_id, user_name = user_name)
      
      if (userExists) {
        
        con = openeo.server$getConnection()
        if (!is.null(user_id)) {
          
          user_info = dbGetQuery(con, "select * from user where user_id = :id"
                                 ,param = list(id=user_id))
          
          
        } else if (!is.null(user_name)){
          user_info = dbGetQuery(con, "select * from user where user_name = :name"
                                 ,param = list(name=user_name))
        } 
        dbDisconnect(con)  
        
        self$user_id = user_info$user_id
        self$user_name = user_info$user_name
        
        invisible(self)
      } else {
        stop("Cannot load user. It doesn't exists or too many user entries.")
      }
      
    },
    store = function() {
      if (is.null(self$user_name) || is.null(self$password)) {
        stop("Cannot create user there are missing information.")
      }
      
      if (!exists.User(user_name=self$user_name)) {
        self$user_id = private$newUserId()
        
        con = openeo.server$getConnection()
        insertUserQuery = "insert into user 
        (user_id, user_name, password) values 
        (:id, :name, :password)"
        
        dbExecute(con,insertUserQuery,param=list(
          id = self$user_id,
          name = self$user_name,
          password = self$password
        ))
        dbDisconnect(con)
        
        if (!dir.exists(self$workspace)) {
          dir.create(self$workspace, showWarnings = FALSE,recursive = TRUE)
        }
        
        user.files.folder = paste(self$workspace, private$files.folder, sep="/")
        if (!dir.exists(user.files.folder)) {
          dir.create(user.files.folder, showWarnings = FALSE,recursive = TRUE)
        }

        invisible(self)
      } else {
        message("Skipping creation. User already exists.")
        invisible(self)
      }
    },
    remove = function() {
      if (is.null(self$user_id) || !exists.User(user_id = self$user_id)) {
        stop("Cannot delete user because it is not instantiated or user does not exists.")
      }
      
      unlink(self$workspace,recursive = TRUE)
      con = openeo.server$getConnection()
      dbExecute(con, "delete from user where user_id = :id",param=list(id=self$user_id))
      dbDisconnect(con)
    }
    
  ),
  # actives ====
  active = list(
    workspace = function() {
      return(paste(openeo.server$workspaces.path,"users",self$user_id,sep="/"))
    },
    files = function() {
      workspace = paste(self$workspace,private$files.folder,sep="/")
      
      relPath=list.files(workspace,recursive = TRUE)
      fileInfos=file.info(list.files(workspace,recursive = TRUE,full.names = TRUE))
      fileInfos$link = relPath
      
      return(fileInfos[,c("link","size")])
    },
    
    jobs = function() {
      con = openeo.server$getConnection()
      result = dbGetQuery(con, "select job_id from job where user_id = :id",param=list(id = self$user_id))
      dbDisconnect(con)
      if (is.null(result)) {
        return(list())
      } else {
        return(as.list(result)[[1]])
      }
    },
    services = function() {
      con = openeo.server$getConnection()
      result = dbGetQuery(con, "select service_id from job as j join service as s on j.job_id = s.job_id where user_id = :id",param=list(id = self$user_id))
      dbDisconnect(con)
      if (is.null(result)) {
        return(list())
      } else {
        return(as.list(result)[[1]])
      }
    }
  ),
  # private ----
  private = list(
    # attributes ====
    jobs.folder = "jobs",
    files.folder = "files",
    
    #function ====
    newUserId = function() {
      id = runif(1, 10^8, (10^9-1))
      
      userIdExists = exists.User(user_id = id)

      if (userIdExists) {
        return(private$newUserId())
      } else {
        return(floor(id))
      }
    }
  )
)

# static ----
#' @export
is.User = function(obj) {
  return(all("User" %in% class(obj)))
}

#' @export
exists.User = function(user_id = NULL, user_name=NULL) {
  if (is.null(user_id) && is.null(user_name)) {
    stop("Cannot search for user without any information.")
  }
  con = openeo.server$getConnection()
  if (is.null(user_id)) {
    queryByName = "select count(user_id) from user where user_name = :name"
    exists = dbGetQuery(con,
                        queryByName,
                        param=list(name=user_name)) == 1
  } else {
    queryById = "select count(user_id) from user where user_id = :id"
    exists = dbGetQuery(con,
                        queryById,
                        param=list(id = user_id)) == 1
  }
  dbDisconnect(con)
  return(exists)
}