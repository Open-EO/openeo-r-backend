#' @export
MapServerLayer <- R6Class(
  "MapServerLayer",
  # public ----
  public=list(
    # attributes ====
    NAME = NULL,
    DATA = NULL,
    STATUS = NULL,
    TYPE = NULL,
    PROJECTION = NULL,
    PROCESSING = NULL,
    
    CONNECTIONTYPE = NULL,
    CONNECTION = NULL,
    
    initialize = function() {
      
    },
    
    toString = function() {
      text = "LAYER"
      
      layer.properties = character(0)
      
      if (!is.null(self$NAME)) {
        layer.properties = append(layer.properties, paste("NAME", "\t", "\"", self$NAME, "\"", sep=""))
      }
      
      # use connectiontype and connection for custom data access, especially by vector data, DATA then becomes the layer name
      if (!is.null(self$CONNECTIONTYPE)) {
        layer.properties = append(layer.properties, paste("CONNECTIONTYPE", "\t",self$CONNECTIONTYPE, sep=""))
      }
      if (!is.null(self$CONNECTION)) {
        layer.properties = append(layer.properties, paste("CONNECTION", "\t", "\"", self$CONNECTION, "\"", sep=""))
      }
      
      if (!is.null(self$DATA)) {
        layer.properties = append(layer.properties, paste("DATA", "\t", "\"",self$DATA, "\"", sep=""))
      }
      if (!is.null(self$STATUS)) {
        layer.properties = append(layer.properties, paste("STATUS", "\t", self$STATUS, sep=""))
      }
      if (!is.null(self$TYPE)) {
        layer.properties = append(layer.properties, paste("TYPE", "\t", self$TYPE, sep=""))
      }
      if (!is.null(self$PROCESSING)) {
        for (option in self$PROCESSING) {
          layer.properties = append(layer.properties, paste("PROCESSING","\t","\"",option,"\"",sep=""))
        }
      }
      
      if (!is.null(self$PROJECTION)) {
        layer.properties = append(layer.properties,"PROJECTION")
        layer.properties = append(layer.properties, paste("\t\"",self$PROJECTION,"\"",sep=""))
        layer.properties = append(layer.properties,"END")
      }
      
      text = append(text, paste("\t",layer.properties, sep=""))
      
      text = append(text, "END")
      return(text)
    }
    
  )
)


# statics ----

#' Checks an object is a MapServerLayer
#' 
#' Calls the class operation on the object and checks if it is of type MapServerLayer
#' 
#' @param obj an object to be checked
#' @return logical if it is a MapServerLayer object
#' @export
is.MapServerLayer = function(obj) {
  return("MapServerLayer" %in% class(obj))
}