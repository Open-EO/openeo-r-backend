#' @include Collection-class.R
#' @include Band-class.R
#' @importFrom R6 R6Class
#' @export
Product <- R6Class(
        "Product",
        # public ----
        public = list(
          # attributes ====
          id = NULL,
          title = NULL,
          description = NULL,
          source= NULL,
          extent = NULL,
          time = NULL,
          srs=NULL,
          
          # functions ====
          initialize = function(id=NA,title = NA, description=NA,source=NA) {
            self$id = id
            self$title = title
            self$description= description
            self$source=source
          },
          shortInfo = function() {
            list(
              name = private$collection_metadata$name,
              title = private$collection_metadata$title,
              description = private$collection_metadata$description,
              license = private$collection_metadata$license,
              extent = private$collection_metadata$extent,
              links = private$collection_metadata$links
            )
          },
          detailedInfo = function() {
            return(private$collection_metadata)
          },
          deriveMetadata = function() {
            private$collection$sortGranulesByTime()
            
            self$time = paste(private$collection$getMinTime(),private$collection$getMaxTime(),sep="/")
            
            self$extent = private$collection$calculateExtent()
            self$srs = private$collection$getGlobalSRS()
          },
          getBandList = function() {
            bands = self$bands
            names(bands) <- NULL
            lapply(bands, function(b) {
              b$toList()
            })
          },
          getCollection = function() {
            return(private$collection)
          },
          setCollection = function(collection) {
            if (!is.Collection(collection)) {
              stop("Cannot assign non Collection parameter as collection.")
            }
            
            private$collection = collection
          },
          
          setCollectionMetadata=function(md) {
            private$collection_metadata = md
          },
          getCollectionMetadata = function() {
            return(private$collection_metadata)
          },
          addSelfReferenceLink = function() {
            
            selfRefLink = paste(openeo.server$configuration$baseserver.url,
                                "collections",
                                self$id,sep="/")
            
            if (!is.null(private$collection_metadata)) {
              private$collection_metadata$links = c(private$collection_metadata$links,
                                                    list(
                                                      list(
                                                        rel="self",
                                                        href=selfRefLink
                                                      )
                                                    ))
            }
            
            return(self)
          }
          
        ),
        # actives ----
        active = list(
          bands = function() {
            return(private$collection$getBandsMetadata())
          }
        ),
        # private ----
        private = list(
          #attributes ====
          collection_metadata = NULL,
          collection=NULL
        )
)

# statics ====

#' @export
is.Product = function(obj) {
  return("Product" %in% class(obj))
}
