ProcessGraph <- R6Class(
  "ProcessGraph",
  # public ----
  public = list(
    # attributes ====
    graph_id = NULL,
    user_id = NULL,
    process_graph = NULL, #parsed list from "process_graph" key in private$json
    output = NULL, #parsed list from "output" key in private$json
    
    # functions ====
    initialize = function(graph_id = NULL, process_graph=NULL, user_id=NULL) {
      if (!is.null(graph_id)) {
        if (!exists.ProcessGraph(graph_id = graph_id)) {
          stop("Cannot find Process Graph under the given id.")
        }
        self$graph_id = graph_id
        return(self$load())
      }
      
      # only if graph_id was not set
      if (!is.null(process_graph)) {
        if (is.list(process_graph)) {
          private$json = toJSON(process_graph,auto_unbox=TRUE,pretty=TRUE)
          private$json2lists()
        } else if (is.character(process_graph)) {
          private$json = process_graph
          private$json2lists()
        } else {
          stop("Invalid process graph")
        }
      }
      if (!is.null(user_id)) {
        self$user_id = user_id
      }
    },
    
    store = function() {
      if (is.null(private$json)) {
        stop("Cannot store process graph with no or invalid JSON information")
      }
      
      if (is.null(self$graph_id)) {
        self$graph_id = private$newProcessGraphId()
      }
      
      if (is.null(private$json)) {
        if (!is.null(self$process_graph) && is.list(self$process_graph)) {
          private$json = toJSON(self$process_graph,auto_unbox=TRUE,pretty=TRUE)
        } else {
          stop("process_graph is null or no list object")
        }
      }
      con = openeo.server$getConnection()
      insertGraphQuery = "insert 
                      into process_graph (graph_id, user_id, process_graph) 
                      values (:graphId, :userId, :graph)"
      
      dbExecute(con, insertGraphQuery,
                param = list(graphId = self$graph_id, 
                             userId = self$user_id, 
                             graph = encodeProcessGraph(private$json)))
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
        
        private$json = decodeProcessGraph(graph_binary)
        private$json2lists()
      }
      
      return(self)
    },
    remove = function() {
      if (!exists.ProcessGraph(self$graph_id)) {
        stop("Cannot find process graph to delete")
      }
      
      con = openeo.server$getConnection()
      success = dbExecute(con, "delete from process_graph where graph_id = :id",param=list(id=self$graph_id)) == 1
      dbDisconnect(con)
      
      return(success)
    }
  ),
  # private ----
  private = list(
    # attributes ====
    json = NULL, # the raw json file combined from "process_graph" and "output" (more as is from the user upload)
    
    # functions ====
    newProcessGraphId = function() {
      randomString = paste("G",createAlphaNumericId(n=1,length=18),sep="")
      
      if (exists.ProcessGraph(randomString)) {
        return(private$newProcessGraphId())
      } else {
        return(randomString)
      }
    },
    
    json2lists = function() {
      list = fromJSON(private$json, simplifyDataFrame=FALSE)
      
      if ("process_graph" %in% names(list)) {
        self$process_graph = list[["process_graph"]]
      } else {
        stop("No process graph found in process graph json.")
      }
      
      if ("output" %in% names(list)) {
        self$output = list[["output"]]
      }
    }
  )
)

# statics ----
is.ProcessGraph = function(obj) {
  return("ProcessGraph" %in% class(obj))
}

is.graph_id = function(graph_id) {
  if (grepl(x = graph_id,pattern=".*[\\[|\\]|\\{|\\}|\"|_|-].*")) {
    return(FALSE)
  }
  
  if (nchar(graph_id) == 18) { # the old version (deprecated)
    return(TRUE)
  } else if (nchar(graph_id) == 19 && startsWith(graph_id, "G")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

exists.ProcessGraph = function(graph_id) {
  if (! is.graph_id(graph_id)) {
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