# for the process descriptions there are several type definitions required, so we provide some of them
# in this file

.generic_param = function(description,required=NULL, ...) {
  param = list()
  param = append(param,list(description=description))
  
  if (!is.null(required) && !is.na(required)) {
    param = append(param,list(required = required))
  }
  
  
  schema = do.call(.generic_schema,list(...))
  
  param = append(param,list(schema=schema))
  
  return(param)
}

.generic_schema = function(type, format=NULL, items=NULL,examples=NULL) {
  schema = list()
  schema = append(schema,list(type=type))
  
  if (!is.null(format) && !is.na(format)) {
    schema = append(schema, list(format=format))
  }
  
  if (!is.null(examples) && !is.na(examples)) {
    schema = append(schema,list(examples=examples))
  }
  
  if ("arrays" %in% tolower(type)) {
    if (!is.null(items) && !is.na(items)) {
      schema = append(schema,list(items=items))
    }
  }
  
  return(schema)
}

result.eodata = .generic_param(description = "Processed EO data.",
                               type="object",
                               format="eodata")


result.vector = .generic_param(description="Processed Vector data derived from extraction of EOData.",
                               type="object",
                               format="vector")
  

  