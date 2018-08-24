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
          
          self$process_graph = fromJSON(private$json, simplifyDataFrame=FALSE)
        } else if (is.character(process_graph)) {
          private$json = process_graph
          self$process_graph = fromJSON(private$json, simplifyDataFrame=FALSE)
        } else {
          stop("Invalid process graph")
        }
      }
      if (!is.na(user_id)) {
        self$user_id = user_id
      }
      
      if (is.null(title)) title = NA
      if (!is.na(title)) {
        self$title = title
      }
      
      if (is.null(description)) description = NA
      if (!is.na(description)) {
        self$description = description
      }
    },
    
    update = function() {
      if (is.null(private$json)) {
        if (!is.null(self$process_graph) && is.list(self$process_graph)) {
          private$json = toJSON(self$process_graph,auto_unbox=TRUE,pretty=TRUE)
        } else {
          stop("process_graph is null or no list object")
        }
      }
      
      con = openeo.server$getConnection()
      updateGraphQuery = "update process_graph set process_graph = :graph, title = :title, description = :description
      where graph_id = :graphId and user_id = :userId"
      
      dbExecute(con, updateGraphQuery,
                param = list(graphId = self$graph_id, 
                             userId = self$user_id, 
                             graph = encodeChar2Hex(private$json),
                             title = self$title,
                             description = self$description))
      dbDisconnect(con)
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
                             graph = encodeChar2Hex(private$json),
                             title = self$title,
                             description = self$description))
      dbDisconnect(con)
      
      return(self)
    },
    load = function() {
      # note: this is a function to load the process graph from db; NOT the one where the process graph is created
      if (exists.ProcessGraph(self$graph_id)) {
        con = openeo.server$getConnection()
        row = dbGetQuery(con, "select title,description,process_graph,user_id from process_graph where graph_id = :id",
                                  param = list(id = self$graph_id))[1,]
        dbDisconnect(con)
        
        if (is.na(self$title)) {
          self$title = row[["title"]]
        }
        
        if (is.na(self$description)) {
          self$description = row[["description"]]
        }
        
        if (is.na(self$user_id)) {
          self$user_id = row[["user_id"]]
        }
        
        if (is.null(self$process_graph)) {
          graph_binary = row[["process_graph"]]
          
          private$json = decodeHex2Char(graph_binary)
          self$process_graph = fromJSON(private$json, simplifyDataFrame=FALSE)
        }
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
    shortInfo = function() {
      return(list(
        process_graph_id = self$graph_id,
        title = self$title,
        description = self$description
      ))
    },
    detailedInfo = function() {
      return(list(
        process_graph_id = self$graph_id,
        title = self$title,
        description = self$description,
        process_graph = self$process_graph
      ))
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
    # dots for user and job
    loadProcess = function(graph_list, ...) {
      # from job
      
      processId = graph_list[["process_id"]]
      graph_list[["process_id"]] = NULL
      graph_list[["porcess_description"]] = NULL
      #now graph list contains only the arguments
      
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
      args = graph_list
      
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
          runner$setArgumentValue(key, private$loadProcess(value,...))
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

encodeChar2Hex = function(text) {
  return(bin2hex(charToRaw(text)))
}

decodeHex2Char = function(hex) {
  return(rawToChar(hex2bin(hex)))
}