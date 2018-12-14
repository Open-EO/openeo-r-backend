#' @importFrom jsonlite validate
NULL

#
# Request handling functions ====
#
.userInformation = function(req,res) {
  user = req$user
  return(user$shortInfo())
}
