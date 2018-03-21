#' @docType class
#' @importFrom R6 R6Class
#' @export
User <- R6Class(
  "User",
  public = list(
    # attributes ====
    user_id = NULL,
    user_name = NULL,
    password = NULL,
    token = NULL,
    
    #public ====
    initialize = function(user_id) {
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
    
    getJobOutputFolder = function(job_id) {
      return(paste(openeo.server$workspaces.path,private$jobs.folder,job_id,sep="/"))
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
    }
  ),
  # private ====
  private = list(
    jobs.folder = "jobs",
    files.folder = "files"
  )
)

# static ====
isUser = function(obj) {
  return(all("User" %in% class(obj)))
}