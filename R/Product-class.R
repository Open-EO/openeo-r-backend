#' @include Collection-class.R
#' @include Band-class.R
#' @importFrom R6 R6Class
#' @export
Product <- R6Class(
        "Product",
        # public ----
        public = list(
          # attributes ====
          product_id = NULL,
          description = NULL,
          source= NULL,
          extent = NULL,
          time = NULL,
          srs=NULL,
          
          # functions ====
          initialize = function(product_id=NA,description=NA,source=NA) {
            self$product_id = product_id
            self$description= description
            self$source=source
          },
          shortInfo = function() {
            list(
              product_id=self$product_id,
              description=self$description,
              source=self$source
            )
          },
          detailedInfo = function() {
            ext=self$extent
            
            list(
              product_id=self$product_id,
              description=self$description,
              source=self$source,
              extent=list(left=xmin(ext),right=xmax(ext),bottom=ymin(ext),top=ymax(ext),srs=toString(self$srs)),
              time=self$time,
              bands=self$getBandList()
            )
          },
          deriveMetadata = function() {
            private$collection$sortGranulesByTime()
            
            self$time = list(from = private$collection$getMinTime(),
                             to = private$collection$getMaxTime())
            
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
          collection=NULL
        )
)

# statics ====

#' @export
is.Product = function(obj) {
  return("Product" %in% class(obj))
}
