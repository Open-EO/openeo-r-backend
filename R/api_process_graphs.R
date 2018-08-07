#
# endpoint function ====
#
createProcessGraphsEndpoint = function() {
  process_graphs = plumber$new()
  
  openeo.server$registerEndpoint("/process_graphs/","POST")
  process_graphs$handle("POST",
            "/",
            handler = .createProcessGraph,
            serializer = serializer_unboxed_json())
  process_graphs$handle("OPTIONS",
            "/",
            handler = .cors_option_bypass)
  
  openeo.server$registerEndpoint("/process_graphs/","GET")
  process_graphs$handle("GET",
                        "/",
                        handler = .listUserProcessGraphs,
                        serializer = serializer_unboxed_json())
  process_graphs$handle("OPTIONS",
                        "/",
                        handler = .cors_option_bypass)
  
  process_graphs$filter("authorization",.authorized)
  
  return(process_graphs)
}

#
# Request handling functions ====
#

.createProcessGraph = function(req,res) {
  if (!is.null(req$postBody) && validate(req$postBody)) {
    process_graph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
    
    graph = ProcessGraph$new(process_graph = process_graph[["process_graph"]], 
                             user_id = req$user$user_id,
                             title = process_graph[["title"]],
                             description = process_graph[["description"]])
    graph$store()
    
    # TODO change to 201 and a location header to process_graph detail
    res$status = 200
    return(list(process_graph_id=graph$graph_id))
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
    
    return(listInfo)
  } else {
    return(list())
  }
  
  
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
  parsedGraph = fromJSON(req$postBody,simplifyDataFrame = FALSE)
  parsedGraph = .createSimpleArgList(parsedGraph)
  
  #fetch old one from db... contains process_graph and output...
  con = openeo.server$getConnection()
  findQuery = "select process_graph from process_graph where graph_id = :gid and user_id = :uid"
  oldBinaryGraph = dbGetQuery(con, findQuery, param=list(gid = graph_id, uid = user_id))[1,]
  oldGraph = fromJSON(decodeProcessGraph(oldBinaryGraph),simplifyDataFrame = FALSE)
  
  oldGraph$process_graph = parsedGraph
  
  binary_processgraph = encodeProcessGraph(toJSON(oldGraph,auto_unbox=TRUE,pretty = TRUE))
  
  query = "update process_graph set process_graph = :binary_graph where graph_id = :gid and user_id = :uid"
  dbExecute(con, query, param = list(binary_graph = binary_processgraph, gid = graph_id, uid = user_id))
  dbDisconnect(con)
  
  return(ok(res))
}