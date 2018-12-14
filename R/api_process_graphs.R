#
# /process_graphs handler functions ----
#

.createProcessGraph = function(req,res) {
  tryCatch({
    if (!is.null(req$postBody) && validate(req$postBody)) {
      process_graph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
      
      graph = ProcessGraph$new(process_graph = process_graph[["process_graph"]], 
                               user_id = req$user$user_id,
                               title = process_graph[["title"]],
                               description = process_graph[["description"]])
      graph$store()
      
      res$setHeader(name = "Location",value=paste(openeo.server$baseserver.url,"process_graphs/",graph$graph_id,sep=""))
      res$setHeader(name = "OpenEO-Identifier",value=graph$graph_id)
      
      res$status = 201
      
      return(res)
    } else {
      throwError("ProcessGraphMissing")
      #No data or malformed json was send"
    }
  }, error=handleError)
  
}

# GET /api/users/<userid>/process_graphs
.listUserProcessGraphs = function(req, res) {
  tryCatch({
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
  }, error = handleError)
  
}

# DELETE /api/process_graphs/<graph_id>
.deleteProcessGraph = function(req,res,process_graph_id) {
  tryCatch({
    user_id = req$user$user_id
    
    con = openeo.server$getConnection()
    query = "delete from process_graph where graph_id = :gid and user_id = :uid"
    success = dbExecute(con,query,param=list(gid = process_graph_id, uid = user_id)) == 1
    
    dbDisconnect(con)
    if (success) {
      res$status = 204
      
      return(res)
    } else {
      throwError("ProcessGraphNotFound")
      # Cannot delete graph. Either it is already deleted, 
      # does not exists or you don't have rights to delete the graph.
    }
  }, error = handleError)
  
}

# GET /api/users/<userid>/process_graphs/<graph_id>
.getProcessGraph = function(req,res,userid,process_graph_id) {
  tryCatch({
    if (!exists.ProcessGraph(process_graph_id)) {
      throwError("ProcessGraphNotFound")
    }
    
    process_graph = ProcessGraph$new(graph_id = process_graph_id)
    process_graph$load()
    
    return(process_graph$detailedInfo())
  }, error=handleError)
  
}

#PATCH /process_graphs/<process_graph_id>
.modifyProcessGraph = function(req,res,process_graph_id) {
  tryCatch({
    if (!exists.ProcessGraph(process_graph_id)) {
      throwError("ProcessGraphNotFound")
    }
    
    if (!process_graph_id %in% req$user$process_graphs) {
      throwError("ProcessGraphNotFound")
    }
    
    parsedGraph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
    
    if ("process_graph" %in% names(parsedGraph)) {
      process_graph = ProcessGraph$new(graph_id = process_graph_id,process_graph = parsedGraph[["process_graph"]])
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
    
    return(res)
  },error=handleError)
}

.validateProcessGraph = function(req,res) {
  tryCatch({
    tryCatch({
      parsedGraph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
    },error=function(e){
      throwError("ProcessGraphMissing")
    })
    
    
    if ("process_graph" %in% names(parsedGraph)) {
      process_graph = ProcessGraph$new(process_graph = parsedGraph[["process_graph"]])
    } else {
      throwError("ProcessGraphMissing")
    }
    
    tryCatch({
      process_graph$buildExecutableProcessGraph(user = req$user)
      res$status = 204
      
      
    },error=function(e) {
      throwError("Internal",message="Process graph contains errors...") #TODO improve!
    })
    
    return(res)
  }, error= handleError)
  
}

