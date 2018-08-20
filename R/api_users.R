#' @importFrom jsonlite validate
NULL

#
# endpoint function ----
#
createMeEndpoint = function() {
  me = plumber$new()
  
  # user information ====
  openeo.server$registerEndpoint("/me/","GET")
  me$handle("GET",
              "/",
              handler = .userInformation,
              serializer = serializer_unboxed_json())
  me$handle("OPTIONS",
              "/",
              handler = .cors_option_bypass)
  
  me$filter("authorization",.authorized)
  
  return(me)
}


#
# Request handling functions ====
#
.userInformation = function(req,res) {
  user = req$user
  return(user$shortInfo())
}
