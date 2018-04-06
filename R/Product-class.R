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
          bands = NULL,
          srs=NULL,
          
          # functions ====
          initialize = function(product_id=NA,description=NA,source=NA) {
            self$product_id = product_id
            self$description= description
            self$source=source
            private$collection = Collection$new()
            self$bands = list()
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
          addGranule = function(granule) {
            private$collection$addGranule(granule=granule)
          },
          deriveMetadata = function() {
            private$collection$sortGranulesByTime()
            
            self$time = list(from = private$collection$getMinTime(),
                             to = private$collection$getMaxTime())
            
            self$extent = private$collection$calculateExtent()
            self$srs = private$collection$getGlobalSRS()
            
            firstGranule = self$getCollection()$granules[[1]]
            if (is.null(self$bands) || length(self$bands) == 0) {
              self$bands = firstGranule$bands
            }
            
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
