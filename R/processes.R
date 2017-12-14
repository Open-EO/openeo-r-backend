#' @include config.R
#' @include Process-class.R

registerProcesses = function() {
  openeo$processes = list()
  
  filter_daterange = Process$new(
    process_id = "filter_daterange",
    description = "filters a data set with a temporal dimension based on a stated start and end date",
    args = list(
      Argument$new(
        name = "imagery",
        description = "the temporal dataset/collection",
        required = TRUE
      ),
      Argument$new(
        name = "from",
        description = "start date/timestamp for the query interval",
        required = FALSE
      ),
      Argument$new(
        name = "to",
        description = "end date/timestamp for the query interval",
        required = FALSE
      )
    ),
    operation = function(imagery, from=NA, to=NA) {
      #imagery might be an identifier or a function (Process$execute()) or a json process description or a
      # udf or a collection we need to specify that
      collection = NULL
      if ("Product" %in% class(imagery)) {
        collection = imagery$getCollection()
      } else if ("Collection" %in% class(imagery)) {
        collection = imagery
      } else if (class(imagery) == "character") {
        #load image or create process
      } else if (class(imagery) == "list") {
        if ("product_id" %in% names(imagery)) {
          collection = openeo$data[[imagery$product_id]]$getCollection()
        }
      }
      if (is.null(collection)) {
        stop("no collection element found in function call")
      }
      
      if (!is.null(from) && class(from) == "character") {
        from = as.POSIXct(from)
      }
      
      if (!is.null(to) && class(to) == "character") {
        to = as.POSIXct(to)
      }
      
      # collection is at this point a Collection
      collection$filterByTime(from=from, to=to)
    }
  )
  filter_daterange$register()
  
}

