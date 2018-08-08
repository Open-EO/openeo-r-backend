#
# endpoint function ====
#

createFilesEndpoint = function() {
  files = plumber$new()
  
  openeo.server$registerEndpoint("/files/{user_id}","GET")
  files$handle("GET",
               "/<user_id>",
               handler = .listUserFiles,
               serializer = serializer_unboxed_json())
  
  openeo.server$registerEndpoint("/files/{user_id}/{path}","GET")
  files$handle("GET",
               "/<user_id>/<path>",
               handler = .downloadUserFile,
               serializer = serializer_unboxed_json())
  
  openeo.server$registerEndpoint("/files/{user_id}/{path}","PUT")
  files$handle("PUT",
               "/<user_id>/<path>",
               handler = .uploadFile,
               serializer = serializer_unboxed_json())
  
  openeo.server$registerEndpoint("/files/{user_id}/{path}","DELETE")
  files$handle("DELETE",
               "/<user_id>/<path>",
               handler = .deleteUserFile,
               serializer = serializer_unboxed_json())
  
  
  files$filter("authorization",.authorized)
  
  return(files)
}

#
# Request handling functions ====
#

#* @get /api/users/<userid>/files
#* @serializer unboxedJSON
.listUserFiles = function(req,res,user_id) {
  if (paste(user_id) == paste(req$user$user_id)) {
    req$user$fileList()
  } else {
    error(res,401,"Not authorized to access other workspaces")
  }
}

#* @get /api/users/<userid>/files/<path>
#* @serializer unboxedJSON
.downloadUserFile = function(req,res,user_id,path) {
  if (paste(user_id) == paste(req$user$user_id)) {
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

# @put /api/users/<userid>/files/<path>
# @serializer unboxedJSON
.uploadFile = function(req,res,user_id,path) {
  if (paste(user_id) == paste(req$user$user_id)) {
    
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
.deleteUserFile = function(req,res,user_id,path) {
  if (paste(user_id) == paste(req$user$user_id)) {
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