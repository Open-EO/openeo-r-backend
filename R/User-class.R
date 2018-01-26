#' @docType class
#' @importFrom R6 R6Class
#' @export
User <- R6Class(
  "User",
  public = list(
    user_id = NULL,
    user_name = NULL,
    password = NULL,
    token = NULL,
    
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
    }
  ),
  active = list(
    workspace = function() {
      return(paste(openeo.server$workspaces.path,self$user_id,sep="/"))
    },
    files = function() {
      workspace = paste(self$workspace,private$files.folder,sep="/")
      
      relPath=list.files(workspace,recursive = TRUE)
      fileInfos=file.info(list.files(workspace,recursive = TRUE,full.names = TRUE))
      fileInfos$link = relPath
      
      return(fileInfos[,c("link","size")])
    },
    
    jobs = function() {
      user.jobs = paste(self$workspace,private$jobs.folder,sep="/")
      return(list.files(user.jobs))
    }
  ),
  private = list(
    jobs.folder = "jobs",
    files.folder = "files"
  )
)

isUser = function(obj) {
  return(all("User" %in% class(obj)))
}