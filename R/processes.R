#' @include config.R
#' @include Process-class.R

registerProcesses = function() {
  openeo$processes = list()
  
  .registerProcess(name="filter_daterange",
                   process=Process$new(
                     process_id="filter_daterange",
                     description="filters a data set with a temporal dimension based on a stated start and end date",
                     args=list(
                       Argument$new(name="imagery",
                                    description="the temporal dataset/collection",
                                    required=TRUE),
                       Argument$new(name="from",
                                    description="start date/timestamp for the query interval",
                                    required=FALSE),
                       Argument$new(name="to",
                                    description="end date/timestamp for the query interval",
                                    required=FALSE)
                     ),
                     execute = function(imagery,from,to) {
                       #imagery might be an identifier or a function (Process$execute()) or a json process description or a
                       # udf or a collection we need to specify that 
                       
                       if ("Product" %in% class(imagery)) {
                         collection = imagery$getCollection()
                       } else if ("Collection" %in% class(imagery)) {
                         collection = imagery
                       } else if (class(imagery) == "character") {
                         #load image or create process
                       }
                       
                       # collection is at this point a Collection
                       
                     }
                   ))
  
}

.registerProcess = function (name,process) {
  if (class(process) != "list") {
    addable = list(process)
  } else {
    addable = process
  }
  names(addable)=c(name)
  
  
  openeo$processes = append(openeo$processes,addable)
}