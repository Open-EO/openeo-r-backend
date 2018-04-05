#' @importFrom jsonlite validate
NULL

#
# endpoint function ====
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
  
  users$handle("GET",
               "/<userid>/services",
               handler = .listUserServices,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/services",
               handler = .cors_option_bypass)
  
  users$handle("GET",
               "/<userid>/process_graphs",
               handler = .listUserProcessGraphs,
               serializer = serializer_unboxed_json())
  users$handle("POST",
               "/<userid>/process_graphs",
               handler = .createUserProcessGraph,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/process_graphs",
               handler = .cors_option_bypass)
  
  
  users$handle("GET",
               "/<userid>/process_graphs/<graph_id>",
               handler = .getProcessGraph,
               serializer = serializer_unboxed_json())
  users$handle("PUT",
               "/<userid>/process_graphs/<graph_id>",
               handler = .modifyProcessGraph,
               serializer = serializer_unboxed_json())
  users$handle("DELETE",
               "/<userid>/process_graphs/<graph_id>",
               handler = .deleteProcessGraph,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/process_graphs/<graph_id>",
               handler = .cors_option_bypass)
  
  
  users$handle("GET",
               "/<userid>/services",
               handler = .listUserServices,
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


#
# Request handling functions ====
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
      return(job$shortInfo())
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

# GET /api/users/<userid>/process_graphs
.listUserProcessGraphs = function(req, res, userid) {
  con = openeo.server$getConnection()
  user_id = req$user$user_id
  query = "select distinct graph_id from process_graph where user_id = :uid"
  graph_ids = dbExecute(con,query,param=list(uid = user_id))
  dbDisconnect(con)
  
  return(graph_ids)
}

# DELETE /api/users/<userid>/process_graphs/<graph_id>
.deleteProcessGraph = function(req,res,userid,graph_id) {
  user_id = req$user$user_id
  
  con = openeo.server$getConnection()
  query = "delete from process_graph where graph_id = :gid and user_id = :uid"
  success = dbExecute(con,query,param=list(gid = graph_id, uid = user_id)) == 1
  
  dbDisconnect(con)
  if (success) {
    ok(res)
  } else {
    error(res = res, status = 500, msg = "Cannot delete graph. Either it is already deleted, does not exists or you don't have rights to delete the graph.")
  }
}

# GET /api/users/<userid>/process_graphs/<graph_id>
.getProcessGraph = function(req,res,userid,graph_id) {
  user_id = req$user$user_id
  
  con = openeo.server$getConnection()
  query = "select process_graph from process_graph where graph_id = :gid and user_id = :uid"
  graph_binary = dbGetQuery(con,query,param=list(gid = graph_id, uid = user_id))[1,]
  
  dbDisconnect(con)
  
  graph_list = fromJSON(decodeProcessGraph(graph_binary),simplifyDataFrame = FALSE)
  return(graph_list$process_graph)
}

#PUT /users/<userid>/process_graphs/<graph_id>
.modifyProcessGraph = function(req,res,userid,graph_id) {
  user_id = req$user$user_id
  parsedGraph = fromJson(req$postBody,simplifyDataFrame = FALSE)
  parsedGraph = .createSimpleArgList(parsedGraph)
  
  #fetch old one from db... contains process_graph and output...
  con = openeo.server$getConnection()
  findQuery = "select process_graph from process_graph where graph_id = :gid and user_id = :uid"
  oldBinaryGraph = dbGetQuery(con, findQuery, param=list(gid = graph_id, uid = user_id))[1,]
  oldGraph = fromJSON(decodeProcessGraph(oldBinaryGraph),simplifyDataFrame = FALSE)
  
  oldGraph$process_graph = parsedGraph
  
  binary_processgraph = encodeProcessGraph(toJSON(oldGraph,auto_unbox=TRUE,pretty = TRUE))
  
  query = "update process_graph set process_graph = :binary_graph where graph_id = :gid and user_id = :uid"
  dbExecute(con, query, param = list(binary_graph = binary_graph, gid = graph_id, uid = user_id))
  dbDisconnect(con)
  
  return(ok(res))
}

.listUserServices = function(req,res,userid) {
  
  if (userid == "me" || userid == req$user$user_id) {
    lapply(req$user$services, function(service_id) {
      return(Service$new()$load(service_id)$detailedInfo())
    })
  }
}