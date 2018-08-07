ProcessGraph <- R6Class(
  "ProcessGraph",
  inherit = DatabaseEntity,
  # public ----
  public = list(
    # attributes ====
    graph_id = NA,
    user_id = NA,
    title = NA,
    description = NA,
    process_graph = NULL, #parsed list from "process_graph" key in private$json
    
    # TODO move to job
    output = NULL, #parsed list from "output" key in private$json
    
    # functions ====
    initialize = function(graph_id = NA, process_graph=NULL, user_id=NA, title=NA,description = NA) {
      if (!is.na(graph_id)) {
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
      if (!is.na(user_id)) {
        self$user_id = user_id
      }
      
      if (!is.na(title)) {
        self$title = title
      }
      
      if (!is.na(description)) {
        self$description = description
      }
    },
    
    store = function() {
      if (is.null(private$json)) {
        stop("Cannot store process graph with no or invalid JSON information")
      }
      
      if (is.na(self$graph_id)) {
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
                      into process_graph (graph_id, user_id, process_graph, title, description) 
                      values (:graphId, :userId, :graph, :title, :description)"
      
      dbExecute(con, insertGraphQuery,
                param = list(graphId = self$graph_id, 
                             userId = self$user_id, 
                             graph = encodeProcessGraph(private$json),
                             title = self$title,
                             description = self$description))
      dbDisconnect(con)
      
      return(self)
    },
    load = function() {
      # note: this is a function to load the process graph from db; NOT the one where the process graph is created
      if (exists.ProcessGraph(self$graph_id)) {
        con = openeo.server$getConnection()
        row = dbGetQuery(con, "select title,description,process_graph from process_graph where graph_id = :id",
                                  param = list(id = self$graph_id))[1,]
        dbDisconnect(con)
        
        self$title = row[["title"]]
        self$description = row[["description"]]
        graph_binary = row[["process_graph"]]
        
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
    },
    
    buildExecutableProcessGraph = function(user=NULL,job=NULL) {
      res = private$loadProcess(self$process_graph,user=user, job=job)
      return(res)
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
      # list is already the process_graph element (w/o title and description)
      list = fromJSON(private$json, simplifyDataFrame=FALSE)
      
      
      self$process_graph = list
      
      
      # TODO move to jobs
      if ("output" %in% names(list)) {
        self$output = list[["output"]]
      }
    },
    # dots for user and job
    loadProcess = function(graph_list, ...) {
      # from job
      
      processId = graph_list[["process_id"]]
      #TODO: add cases for udfs
      if (!is.null(processId) && processId %in% names(openeo.server$processes)) {
        process = openeo.server$processes[[processId]]
        
        return(private$as.executable(graph_list=graph_list,process=process, ...))
      } else {
        stop(paste("Cannot load process",processId))
      }
    },
    as.executable = function(graph_list, process, ...) {
      # from process
      
      if (is.null(process) && ! is.Process(process)) {
        stop("No process defined to create executable from. Reason: null or no Process")
      }
      #return a process where the arguments from the parsed json file are set for
      #this "args". E.g. set a value for args[["from"]]$value
      
      # graph_list: at this point is the named list of the process graph
      args = graph_list$args
      
      runner = process$clone(deep=TRUE)
      
      clonedArguments = list()
      #deep copy also the arguments
      for (arg in process$args) {
        clonedArguments=append(clonedArguments,arg$clone(deep=TRUE))
      }
      runner$args = clonedArguments
      
      for (key in names(args)) {
        value = args[[key]]
        
        #TODO maybe add a handling for UDF or in the UDF class 
        if (class(value) == "list" && "process_id" %in% names(value)) {
          runner$setArgumentValue(key, private$loadProcess(value))
        } else {
          runner$setArgumentValue(key, value)
        }
      }
      dots = list(...)
      result = ExecutableProcess$new(process=runner)
      result$job = dots$job
      result$user = dots$user
      return(result)
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