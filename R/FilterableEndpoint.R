
FilterableEndpoint <- R6Class(
  "FilterableEndpoint",
  inherit = PlumberEndpoint,
  # public ====
  public = list(
    initialize = function(verbs, path, expr, envir, serializer, lines, params, comments, responses, tags){ # taken from PlumberEndpoint
      self$verbs <- verbs
      self$path <- path
      
      private$regex <- plumber:::createPathRegex(path)
      
      private$expr <- expr
      if (is.expression(expr)){
        private$func <- eval(expr, envir)
      } else {
        private$func <- expr
      }
      private$envir <- envir
      
      if (!missing(serializer) && !is.null(serializer)){
        self$serializer <- serializer
      }
      if (!missing(lines)){
        self$lines <- lines
      }
      if (!missing(params)){
        self$params <- params
      }
      if (!missing(comments)){
        self$comments <- comments
      }
      if (!missing(responses)){
        self$responses <- responses
      }
      if(!missing(tags) && !is.null(tags)){
        # make sure we box tags in json using I()
        # single tags should be converted to json as:
        # tags: ["tagName"] and not tags: "tagName"
        self$tags <- I(tags)
      }
    },
    
    registerFilter = function(filter) {
      # TODO type check if list check if each element is PlumberStep      
      # if (!inherits(filter,"PlumberStep")) {
      #   stop("No filter")
      # }
      
      private$filts = private$filts %>% append(filter)
    },
    
    exec = function(...){
      args <- list(...)
      
      #modify req / res
      filterResponse = private$runFilters(req=args$req,res=args$res)
      
      if (length(private$filts) > 0 && !openeo.globals$forwarded){
        return(filterResponse)
      }
      
      hookEnv <- new.env()
      
      private$runHooks("preexec", c(list(data=hookEnv), list(...)))
      val <- do.call(private$func, 
                     plumber:::getRelevantArgs(list(...), plumberExpression=private$expr), 
                     envir=private$envir)
      val <- private$runHooks("postexec", c(list(data=hookEnv, value=val), list(...)))
      val
    }
  ),
  private = list(
    filts = list(),
    
    runFilters = function(req,res) {
      
      if (length(private$filts) > 0){
        # Start running through filters until we find a matching endpoint.
        for (i in 1:length(private$filts)){
          fi <- private$filts[[i]]
          # Execute this filter
          openeo.globals$forwarded = FALSE
          
          fres <- do.call(fi$exec, req$args)
          if (!openeo.globals$forwarded){
            # forward() wasn't called, presumably meaning the request was
            # handled inside of this filter.
            if (!is.null(fi$serializer)){
              res$serializer <- fi$serializer
            }
            return(fres)
          }
        }
      }
    }
  )
)

OpenEORouter = R6Class(
  "OpenEORouter",
  inherit = plumber,
  public = list(
    initialize = function(filters=plumber:::defaultPlumberFilters,envir) {
      if (missing(envir)){
        private$envir <- new.env(parent=.GlobalEnv)
      } else {
        private$envir <- envir
      }
      
      if (is.null(filters)){
        filters <- list()
      }
      
      # Add in the initial filters
      for (fn in names(filters)){
        fil <- plumber:::PlumberFilter$new(fn, filters[[fn]], private$envir, private$serializer, NULL)
        private$filts <- c(private$filts, fil)
      }
      
      private$errorHandler <- plumber:::defaultErrorHandler()
      private$notFoundHandler <- plumber:::default404Handler
      
    },
    createFilter = function(name, expr, serializer) {
      return(plumber:::PlumberFilter$new(name, expr, private$envir, serializer))
    },
    createEndpoint = function(methods, path, handler, preempt, serializer, endpoint) {
      epdef <- !missing(methods) || !missing(path) || !missing(handler) || 
        !missing(serializer)
      if (!missing(endpoint) && epdef) {
        stop("You must provide either the components for an endpoint (handler and serializer) OR provide the endpoint yourself. You cannot do both.")
      }
      if (epdef) {
        if (missing(serializer)) {
          serializer <- private$serializer
        }
        endpoint <- FilterableEndpoint$new(methods, path, handler, 
                                        private$envir, serializer)
        
        return(endpoint)
      }
    }
  )
)
