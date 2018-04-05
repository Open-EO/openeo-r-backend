ProcessGraph <- R6Class(
  "ProcessGraph",
  # public ----
  public = list(
    # attributes ====
    graph_id = NULL,
    user_id = NULL,
    process_graph = NULL,
    
    # functions ====
    initialize = function(process_graph=NULL, user_id=NULL) {
      self$process_graph = process_graph
      self$user_id = user_id
    },
    
    store = function() {
      
      if (is.null(self$process_id)) {
        self$process_id = private$newProcessGraphId()
      }
      
      if (is.list(self$process_graph)) {
        self$process_graph = toJSON(self$process_graph,auto_unbox=TRUE,pretty=TRUE)
      } else {
        stop("process_graph is no list object")
      }
      enc_process_graph = encodeProcessGraph(self$process_graph)
      
      con = openeo.server$getConnection()
      dbExecute(con, "insert into process_graph (graph_id, user_id, process_graph) values (:graphId, :userId, :graph)",
                param = list(graphId = self$process_id, 
                             userId = self$user_id, 
                             graph = enc_process_graph))
      dbDisconnect(con)
      
      return(self)
    },
    load = function() {
      # note: this is a function to load the process graph from db; NOT the one where the process graph is created
      if (exists.ProcessGraph(self$graph_id)) {
        con = openeo.server$getConnection()
        graph_binary = dbGetQuery(con, "select process_graph from process_graph where graph_id = :id",
                                  param = list(id = self$graph_id))[1,]
        dbDisconnect(con)
        graph_list = fromJSON(decodeProcessGraph(graph_binary),simplifyDataFrame = FALSE)
        self$process_graph = graph_list
        
      }
      
      return(self)
    }
  ),
  # private ----
  private = list(
    newProcessGraphId = function() {
      randomString = createAlphaNumericId(n=1,length=18)
      
      if (exists.ProcessGraph(randomString)) {
        return(private$newProcessGraphId())
      } else {
        return(randomString)
      }
    }
  )
)

# statics ----
exists.ProcessGraph = function(graph_id) {
  if (nchar(graph_id) != 18) {
    return (FALSE)
  }
  con = openeo.server$getConnection()
  graphIdExists = dbGetQuery(con, "select count(*) from process_graph where graph_id = :id", 
                             param=list(id=graph_id)) == 1
  dbDisconnect(con)
  
  return(graphIdExists)
}

encodeProcessGraph = function(text) {
  return(bin2hex(charToRaw(text)))
}

decodeProcessGraph = function(hex) {
  return(rawToChar(hex2bin(hex)))
}