test_stop_on_server_error <- function() {

  # Minimal stand-in for an httr response; only the status code is inspected.
  fake_response <- function(status) {
    structure(list(status_code = status), class = "response")
  }
  uri <- "https://string-db.org/api/tsv/enrichment"

  # A gateway timeout must stop instead of returning the error page as data.
  error_message <- tryCatch(
    {
      STRINGdb:::stop_on_server_error(fake_response(524L), uri)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  checkTrue(!is.null(error_message))
  checkTrue(grepl("HTTP status 524", error_message))

  # Successful responses and client errors (e.g. 404 for unknown identifiers)
  # keep their previous behaviour.
  checkTrue(!is.null(STRINGdb:::stop_on_server_error(fake_response(200L), uri)))
  checkTrue(!is.null(STRINGdb:::stop_on_server_error(fake_response(404L), uri)))

}
