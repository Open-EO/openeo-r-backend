library(R6)

Product <- R6Class("Product",
        public = list(
          product_id = NULL,
          description = NULL,
          source= NULL,
          extent = NULL,
          time = NULL,
          bands = NULL,
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
            list(
              product_id=self$product_id,
              description=self$description,
              source=self$source,
              extent=self$extent,
              time=self$time,
              bands=self$bands
            )
          }
          
        )
)
