#' @export
MapServerWeb <- R6Class(
  "MapServerWeb",
  # public ----
  public=list(
    # attributes ====
    METADATA = NULL,
    
    # function ====
    toString = function() {
      text = "WEB"
      web.properties = character(0)
      
      metadata.properties = character(0)
      web.properties = append(web.properties,"METADATA")
      for (key in names(self$METADATA)) {
        value = self$METADATA[[key]]
        metadata.properties = append(metadata.properties,paste("\"",key,"\"","\t","\"",value,"\"",sep=""))
      }
      web.properties = append(web.properties, paste("\t",metadata.properties,sep=""))
      
      web.properties = append(web.properties,"END")
      
      text = append(text, paste("\t",web.properties,sep=""))
      text = append(text,"END")
      return(text)
    }
  ),
  private=list()
)

# statics ----

#' Checks an object is a MapServerWeb
#' 
#' Calls the class operation on the object and checks if it is of type MapServerWeb
#' 
#' @param obj an object to be checked
#' @return logical if it is a MapServerWeb object
#' @export
is.MapServerWeb = function(obj) {
  return("MapServerWeb" %in% class(obj))
}