#
# /process_graphs handler functions ----
#

.createProcessGraph = function(req,res) {
  if (!is.null(req$postBody) && validate(req$postBody)) {
    process_graph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
    
    graph = ProcessGraph$new(process_graph = process_graph[["process_graph"]], 
                             user_id = req$user$user_id,
                             title = process_graph[["title"]],
                             description = process_graph[["description"]])
    graph$store()
    
    res$setHeader(name = "Location",value=paste(openeo.server$baseserver.url,"process_graphs/",graph$graph_id,sep=""))
    # res$setHeader(name = "Authorization", value=req$HTTP_AUTHORIZATION)
    
    res$status = 201
  } else {
    return(error(res,400,"No data or malformed json was send"))
  }
}

# GET /api/users/<userid>/process_graphs
.listUserProcessGraphs = function(req, res) {
  con = openeo.server$getConnection()
  
  graph_ids = req$user$process_graphs
  if (length(graph_ids) > 0 ) {
    listInfo = lapply(graph_ids, function(gid) {
      process_graph = ProcessGraph$new(graph_id = gid)
      process_graph$load()
      return(process_graph$shortInfo())
    })
    
    return(list(process_graphs=listInfo,links=list()))
  } else {
    return(list(process_graphs=list(),link=list()))
  }
  
  
}

# DELETE /api/process_graphs/<graph_id>
.deleteProcessGraph = function(req,res,process_graph_id) {
  user_id = req$user$user_id
  
  con = openeo.server$getConnection()
  query = "delete from process_graph where graph_id = :gid and user_id = :uid"
  success = dbExecute(con,query,param=list(gid = process_graph_id, uid = user_id)) == 1
  
  dbDisconnect(con)
  if (success) {
    res$status = 204
  } else {
    error(res = res, status = 500, msg = "Cannot delete graph. Either it is already deleted, does not exists or you don't have rights to delete the graph.")
  }
}

# GET /api/users/<userid>/process_graphs/<graph_id>
.getProcessGraph = function(req,res,userid,process_graph_id) {
  
  process_graph = ProcessGraph$new(graph_id = process_graph_id)
  process_graph$load()
  
  return(process_graph$detailedInfo())
}

#PATCH /process_graphs/<process_graph_id>
.modifyProcessGraph = function(req,res,process_graph_id) {
  parsedGraph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
  
  if ("process_graph" %in% names(parsedGraph)) {
    process_graph = ProcessGraph$new(graph_id = process_graph_id,process_graph = parsedGraph[["process_graph"]])
  } else {
    process_graph = ProcessGraph$new(graph_id = process_graph_id)
  }
  process_graph$load()
  
  # title
  if (!is.null(parsedGraph[["title"]])) {
    process_graph$title = parsedGraph[["title"]]
  }
  
  # description
  if (!is.null(parsedGraph[["description"]])) {
    process_graph$description = parsedGraph[["description"]]
  }
  
  process_graph$update()
  
  res$status = 204
}

.validateProcessGraph = function(req,res) {
  parsedGraph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
  
  if ("process_graph" %in% names(parsedGraph)) {
    process_graph = ProcessGraph$new(process_graph = parsedGraph[["process_graph"]])
  } else {
    stop("No process graph sent to the backend")
  }
  
  tryCatch({
    process_graph$buildExecutableProcessGraph(user = req$user)
    res$status = 204
    
    
  },error=function(e) {
    return(openEO.R.Backend:::error(res,501,"Process graph contains errors...")) #TODO improve!
  }) 
  
  
}