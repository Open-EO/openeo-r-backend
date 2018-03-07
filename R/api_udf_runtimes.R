createUDFRuntimesEndpoint = function() {
  udf_runtimes = plumber$new()
  
  udf_runtimes$handle("GET",
                      "/",
                      handler = .not_implemented_yet,
                      serializer=serializer_unboxed_json())
  udf_runtimes$handle("OPTIONS",
                      "/",
                      handler = .cors_option_bypass)
  
  udf_runtimes$handle("GET",
                      "/<lang>/<udf_type>",
                      handler = .not_implemented_yet,
                      serializer = serializer_unboxed_json())
  udf_runtimes$handle("OPTIONS",
                      "/<lang>/<udf_type>",
                      handler = .cors_option_bypass)
  
  return(udf_runtimes)
}