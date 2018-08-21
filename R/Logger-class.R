#' @importFrom R6 R6Class
#' @export
Logger <- R6Class(
  "Logger",
  # public ----
  public = list(
    # attributes ====
    process_id = NA,
    # functions ====
    initialize = function(process_id = NA) {
      self$process_id = process_id
      invisible(self)
    },
    info = function(job_id = NA, service_id = NA, message) {
      private$log(job_id=job_id,service_id = service_id, message = message,category = "info")
    },
    error = function(job_id = NA, service_id = NA, message) {
      private$log(job_id=job_id,service_id = service_id, message = message,category = "error")
      stop(message)
    }
  ),
  private = list(
    log = function(job_id = NA, service_id = NA, message, category) {
      con = openeo.server$getConnection()
      tryCatch(
        {
          invisible(dbExecute(con, 
                    "insert into log (timestamp,message,job_id,service_id,category,process_id) 
                    values (:time,:msg,:jid,:sid,:category,:pid)", 
                    param = list(
                      time=iso_datetime(now()),
                      msg = message, 
                      jid=job_id, 
                      sid=service_id,
                      category=category,
                      pid=self$process_id)))
        },
        finally={dbDisconnect(con)}
      )
    }
  )
)