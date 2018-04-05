#' @export
MapServerMap <- R6Class(
  "MapServerMap",
  # public ----
  public=list(
    # attributes ====
    NAME = NULL,
    EXTENT = NULL,
    LAYER = NULL,
    PROJECTION = NULL,
    SIZE = NULL,
    WEB = NULL,
    
    # functions ====
    initialize = function() {
      
    },

    toString = function() {
      data = "MAP"
      
      map.properties = character(0)
      if (!is.null(self$NAME)) {
        line = paste("NAME","\t",self$NAME,sep="")
        map.properties = append(map.properties,line)
      }
      if (!is.null(self$EXTENT)) {
        line = paste("EXTENT","\t",self$EXTENT,sep="")
        map.properties = append(map.properties,line)
      }
      if (!is.null(self$PROJECTION)) {
        map.properties = append(map.properties,paste("PROJECTION",sep=""))
        map.properties = append(map.properties,paste("\t","\"",self$PROJECTION,"\"",sep=""))
        map.properties = append(map.properties,paste("END",sep=""))
      }
      if (!is.null(self$SIZE)) {
        map.properties = append(map.properties,paste("SIZE","\t",self$SIZE,sep=""))
      }
      
      if (!is.null(self$WEB)) {
        map.properties = append(map.properties, self$WEB$toString())
      }
      
      if (!is.null(self$LAYER)) {
        for (key in 1:length(self$LAYER)) {
          map.properties = append(map.properties, self$LAYER[[key]]$toString())
        }
      }
      
      
      data = append(data,paste("\t",map.properties,sep=""))
      data = append(data,"END")
      
      return(data)
    }
    
  ),
  # private ----
  private=list()
)


# statics ----

#' Checks an object is a MapServerMap
#' 
#' Calls the class operation on the object and checks if it is of type MapServerMap
#' 
#' @param obj an object to be checked
#' @return logical if it is a MapServerMap object
#' @export
is.MapServerMap = function(obj) {
  return("MapServerMap" %in% class(obj))
}