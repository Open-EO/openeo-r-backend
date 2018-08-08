#' @importFrom jsonlite validate
NULL

#
# endpoint function ====
#
createMeEndpoint = function() {
  me = plumber$new()
  
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



# TODO remove
createUsersEndpoint = function() {
  users = plumber$new()

  
  users$handle("GET",
               "/<userid>/jobs",
               handler = .listUserJobs,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/jobs",
               handler = .cors_option_bypass)
  
  users$handle("GET",
               "/<userid>/services",
               handler = .listUserServices,
               serializer = serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/services",
               handler = .cors_option_bypass)
  
  
  users$handle("GET",
               "/<userid>/services",
               handler = .listUserServices,
               serializer=serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/services",
               handler = .cors_option_bypass)
  
  users$handle("GET",
               "/<userid>/credits",
               handler = .not_implemented_yet,
               serializer=serializer_unboxed_json())
  users$handle("OPTIONS",
               "/<userid>/credits",
               handler = .cors_option_bypass)
  
  users$filter("authorization",.authorized)
  
  return(users)
  
}


#
# Request handling functions ====
#



.userInformation = function(req,res) {
  user = req$user
  return(user$shortInfo())
}






.listUserServices = function(req,res,userid) {
  
  if (userid == "me" || userid == req$user$user_id) {
    lapply(req$user$services, function(service_id) {
      return(Service$new(service_id)$load()$detailedInfo())
    })
  }
}