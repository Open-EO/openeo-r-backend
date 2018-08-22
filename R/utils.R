#' @importFrom lubridate as_datetime
# function to transcode a datetime string into a ISO 8601 datetime string
iso_datetime = function(string) {
  return(format(as_datetime(string),format="%Y-%m-%dT%H:%M:%SZ"))
}


iso_datetime_milli = function(string) {
  return(format(as_datetime(string),format="%Y-%m-%dT%H:%M:%OS3Z"))
}