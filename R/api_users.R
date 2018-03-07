#' @importFrom jsonlite validate
NULL
############################
#
# user data and functions
#


createUsersEndpoint = function() {
  users = plumber$new()
  
  users$handle("GET",
               "/<userid>/files",
               handler = .listUserFiles,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/files",
               handler = .cors_option_bypass)
  
  
  users$handle("GET",
               "/<userid>/files/<path>",
               handler = .downloadUserFile,
               serializer = serializer_unboxed_json())
  users$handle("PUT",
               "/<userid>/files/<path>",
               handler = .uploadFile,
               serializer = serializer_unboxed_json())
  users$handle("DELETE",
               "/<userid>/files/<path>",
               handler = .deleteUserData,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/files/<path>",
               handler = .cors_option_bypass)
  
  
  users$handle("GET",
               "/<userid>/jobs",
               handler = .listUserJobs,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/jobs",
               handler = .cors_option_bypass)
  
  
  users$handle("POST",
               "/<userid>/process_graphs",
               handler = .createUserProcessGraph,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/process_graphs",
               handler = .cors_option_bypass)
  
  
  users$handle("GET",
               "/<userid>/process_graphs/<graph_id>",
               handler = .not_implemented_yet,
               serializer = serializer_unboxed_json())
  users$handle("DELETE",
               "/<userid>/process_graphs/<graph_id>",
               handler = .not_implemented_yet,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/process_graphs/<graph_id>",
               handler = .cors_option_bypass)
  
  
  users$handle("GET",
               "/<userid>/services",
               handler = .not_implemented_yet,
               serializer=serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/services",
               handler = .cors_option_bypass)
  
  users$handle("GET",
               "/<userid>/credits",
               handler = .not_implemented_yet,
               serializer=serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/credits",
               handler = .cors_option_bypass)
  
  users$filter("authorization",.authorized)
  
  return(users)
  
}

############################
#
# Request handling functions
#

#* @get /api/users/<userid>/files
#* @serializer unboxedJSON
.listUserFiles = function(req,res,userid) {
  if (paste(userid) == paste(req$user$user_id)) {
    req$user$fileList()
  } else {
    error(res,401,"Not authorized to access other workspaces")
  }
}

#* @get /api/users/<userid>/files/<path>
#* @serializer unboxedJSON
.downloadUserFile = function(req,res,userid,path) {
  if (paste(userid) == paste(req$user$user_id)) {
    path = URLdecode(path)
    
    files = req$user$files
    selection = files[files[,"link"]==path,]
    if (nrow(selection) == 0) {
      error(res, 404,paste("File not found under path:",path))
    } else {
      path.ext = unlist(strsplit(selection[,"link"], "\\.(?=[^\\.]+$)", perl=TRUE))
      
      sendFile(
        res,
        200,
        file.name = path.ext[1],
        file.ext = paste(".",path.ext[2],sep=""),
        data = readBin(rownames(selection),"raw", n=selection$size)
      )
    }
  } else {
    error(res,401,"Not authorized to access others files")
  }
  
}

### this creates corrupted files, something along the line read file / write postbody is off

# @put /api/users/<userid>/files/<path>
# @serializer unboxedJSON
.uploadFile = function(req,res,userid,path) {
  if (paste(userid) == paste(req$user$user_id)) {
    
    path = URLdecode(path)
    
    storedFilePath = paste(req$user$workspace,"files",path,sep="/")
    
    if (file.exists(storedFilePath)) {
      file.remove(storedFilePath)
    }
    
    dir.split = unlist(strsplit(storedFilePath, "/(?=[^/]+$)", perl=TRUE))
    
    req$rook.input$initialize(req$rook.input$.conn,req$rook.input$.length)
    
    dir.create(dir.split[1],recursive = TRUE,showWarnings = FALSE)
    file.create(storedFilePath,showWarnings = FALSE)
    
    outputFile = file(storedFilePath,"wb")
    writeBin(req$rook.input$read(req$rook.input$.length), con=outputFile, useBytes = TRUE)
    close(outputFile,type="wb")
    ok(res)
  } else {
    error(res,401,"Not authorized to upload data into other users workspaces")
  }
}

#* @delete /api/users/<userid>/files/<path>
#* @serializer unboxedJSON
.deleteUserData = function(req,res,userid,path) {
  if (paste(userid) == paste(req$user$user_id)) {
    user = req$user
    path = URLdecode(path)
    
    storedFilePath = paste(user$workspace,"files",path,sep="/")
    
    files = req$user$files
    selection = files[files[,"link"]==path,]
    if (nrow(selection) == 0) {
      error(res, 404,paste("User has no file under path:",path))
    } else {
      file = rownames(selection)
      
      unlink(file, recursive = TRUE,force=TRUE)
      ok(res)
    }
  } else {
    error(res,401,"Not authorized to delete data of others")
  }
  
}

#* @get /api/users/<userid>/jobs
#* @serializer unboxedJSON
.listUserJobs = function(req,res,userid) {
  if (paste(userid) == paste(req$user$user_id)) {
    user = req$user

    possibleUserJobs = user$jobs
    jobRepresentation = lapply(possibleUserJobs, function(job_id){
      job = openeo.server$loadJob(job_id)
      return(job$detailedInfo())
    })
    
    return(unname(jobRepresentation))
  } else {
    error(res,401,"Not authorized to view jobs of others")
  }
  
}

# POST /api/users/<userid>/processes
#
.createUserProcessGraph = function(req,res,userid) {
  if (paste(userid) == paste(req$user$user_id)) {
    if (!is.null(req$postBody) && validate(req$postBody)) {
      process_graph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
      process_graph = .createSimpleArgList(process_graph)
      graph_id = openeo.server$createProcessGraph(process_graph, req$user$user_id)
      
      res$status = 200
      return(list(process_graph_id=graph_id))
    } else {
      return(error(res,400,"No data or malformed json was send"))
    }
  } else {
    error(res,401,"Not authorized to view jobs of others")
  }
}