#' @importFrom R6 R6Class
#' @export
Logger <- R6Class(
  "Logger",
  # public ----
  public = list(
    # attributes ====
    # functions ====
    initialize = function(process = NULL, job = NULL, service = NULL) {
      private$process = process
      private$job = job
      private$service = service
      invisible(self)
    },
    info = function(message) {
      private$log(message = message,category = "info")
    },
    error = function(message) {
      private$log(message = message,category = "error")
      stop(message)
    }
  ),
  # private ----
  private = list(
    # attributes ====
    service = NULL,
    job = NULL,
    process = NULL,
    # functions ====
    log = function(message, category) {
      con = openeo.server$getConnection()
      
      if (!is.null(private$process)) {
        process_id = private$process$process_id
      } else {
        process_id = NA
      }
      
      if (!is.null(private$job)) {
        job_id = private$job$job_id
      } else {
        job_id = NA
      }
      
      if (!is.null(private$service)) {
        service_id = private$service$service_id
      } else {
        service_id = NA
      }
      
      tryCatch(
        {
          invisible(dbExecute(con, 
                    "insert into log (timestamp,message,job_id,service_id,category,process_id) 
                    values (:time,:msg,:jid,:sid,:category,:pid)", 
                    param = list(
                      time=iso_datetime_milli(now()),
                      msg = message, 
                      jid=job_id, 
                      sid=service_id,
                      category=category,
                      pid=process_id)))
        },
        finally={dbDisconnect(con)}
      )
    }
  )
)