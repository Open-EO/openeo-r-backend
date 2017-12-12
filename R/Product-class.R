#' @include Collection-class.R
library(R6)

Product <- R6Class("Product",
        public = list(
          product_id = NULL,
          description = NULL,
          source= NULL,
          extent = NULL,
          time = NULL,
          bands = NULL,
          collection=NULL,
          srs=NULL,
          initialize = function(product_id=NA,description=NA,source=NA) {
            self$product_id = product_id
            self$description= description
            self$source=source
            self$collection = Collection$new()
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
              bands=self$bands
            )
          },
          addGranule = function(granule) {
            self$collection$addGranule(granule=granule)
          },
          finalize = function() {
            self$collection$sortGranulesByTime()
            
            self$time = list(from = self$collection$getMinTime(),
                             to = self$collection$getMaxTime())
            
            self$extent = self$collection$calculateExtent()
            self$srs = self$collection$getGlobalSRS()
            
          }
          
        )
)
