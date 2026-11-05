# Offline tests of the helper of the tests that call STRING (helper_live_string.R). STRING is replaced by
# an address that cannot be reached, a closed port of this computer, and by the errors that requests and
# downloads give when STRING cannot be reached.

source(system.file("unitTests", "helper_live_string.R", package = "STRINGdb"), local = TRUE)

# A closed port of this computer: a request to it fails at once with a connection error.
UNREACHABLE_ADDRESS <- "https://127.0.0.1:9"

# Stand-in for RUnit's DEACTIVATED, which would mark this test itself as deactivated: stops with a
# condition of class "live_string_deactivated".
signal_deactivated <- function(msg) {
  stop(structure(class = c("live_string_deactivated", "error", "condition"), list(message = msg, call = NULL)))
}

# Evaluates expression and tells how it ended: kind is "value", "deactivated" or "error", with the
# message of the condition.
outcome_of <- function(expression) {
  tryCatch(
    {
      force(expression)
      list(kind = "value", message = NULL)
    },
    live_string_deactivated = function(cond) list(kind = "deactivated", message = conditionMessage(cond)),
    error = function(e) list(kind = "error", message = conditionMessage(e))
  )
}

# Minimal stand-in for an httr response; stop_on_server_error and stop_on_client_error read only these.
fake_response <- function(status) {
  structure(list(status_code = status, content = charToRaw("Error page")), class = "response")
}


test_live_string_connection_errors <- function() {

  # A request and a download that cannot reach the server.
  request_error <- tryCatch(httr::GET(UNREACHABLE_ADDRESS, httr::timeout(5)), error = function(e) e)
  checkTrue(inherits(request_error, "error"))
  checkTrue(is_string_connection_error(request_error))

  download_warnings <- character(0)
  download_error <- withCallingHandlers(
    tryCatch(
      download.file(paste(UNREACHABLE_ADDRESS, "/9606.protein.info.v12.5.txt.gz", sep = ""), tempfile(), quiet = TRUE),
      error = function(e) e
    ),
    warning = function(w) {
      download_warnings <<- c(download_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  checkTrue(inherits(download_error, "error"))
  checkTrue(is_string_connection_error(download_error, download_warnings))

  # The failures of the Bioconductor build machines, and the messages of the curl package before
  # version 5, whose errors had no class.
  checkTrue(is_string_connection_error(simpleError("SSL connect error")))
  checkTrue(is_string_connection_error(simpleError("Timeout was reached: [version-12-5.string-db.org] Operation timed out after 10001 milliseconds")))
  checkTrue(is_string_connection_error(simpleError("Could not resolve host: stringdb-downloads.org")))
  checkTrue(is_string_connection_error(simpleError("Failed to connect to string-db.org port 443: Connection refused")))

  # The package's own error for an answer with HTTP status 5xx.
  server_error <- tryCatch(
    STRINGdb:::stop_on_server_error(fake_response(503L), "https://version-12-5.string-db.org/api/tsv/enrichment"),
    error = function(e) e
  )
  checkTrue(is_string_connection_error(server_error))

  # A download that the server failed to answer (5xx) is a connection failure; a file that it refused
  # (4xx) is not, as it points at a wrong address.
  download_failure <- simpleError("cannot open URL 'https://stringdb-downloads.org/download/x.txt.gz'")
  checkTrue(is_string_connection_error(
    download_failure,
    "cannot open URL 'https://stringdb-downloads.org/download/x.txt.gz': HTTP status was '503 Service Unavailable'"
  ))
  checkTrue(!is_string_connection_error(
    download_failure,
    "cannot open URL 'https://stringdb-downloads.org/download/x.txt.gz': HTTP status was '404 Not Found'"
  ))

  # Not connection failures: an assertion failure, other errors, a request that curl rejects itself,
  # and STRING's refusal of a request.
  checkTrue(!is_string_connection_error(simpleError("Test not TRUE\n")))
  checkTrue(!is_string_connection_error(simpleError("object 'string_db' not found")))

  malformed_request_error <- tryCatch(httr::GET("https://"), error = function(e) e)
  checkTrue(inherits(malformed_request_error, "error"))
  checkTrue(!is_string_connection_error(malformed_request_error))

  refused_error <- tryCatch(
    STRINGdb:::stop_on_client_error(fake_response(404L), "https://version-12-5.string-db.org/api/svg/network"),
    error = function(e) e
  )
  checkTrue(!is_string_connection_error(refused_error))

}


test_live_string_unreachable_host <- function() {

  reason <- string_unreachable_reason(paste(UNREACHABLE_ADDRESS, "/", sep = ""), timeout_seconds = 5)
  checkTrue(!is.null(reason))
  checkTrue(grepl("127.0.0.1", reason, fixed = TRUE))

  # Without addresses to ask, nothing is unreachable.
  checkTrue(is.null(string_unreachable_reason(character(0))))

  # A test is deactivated before its body when STRING cannot be reached.
  outcome <- outcome_of(
    skip_unless_string_reachable(paste(UNREACHABLE_ADDRESS, "/", sep = ""), timeout_seconds = 5, deactivate = signal_deactivated)
  )
  checkEquals("deactivated", outcome$kind)
  checkTrue(grepl("^STRING could not be reached: https://127.0.0.1:9/", outcome$message))

  body_evaluated <- FALSE
  outcome <- outcome_of(
    with_live_string(
      {
        body_evaluated <- TRUE
      },
      urls = paste(UNREACHABLE_ADDRESS, "/", sep = ""), deactivate = signal_deactivated
    )
  )
  checkEquals("deactivated", outcome$kind)
  checkTrue(!body_evaluated)

}


test_with_live_string <- function() {

  # urls = character(0): STRING is not asked before the body, which only the errors of the body decide.
  # The download below goes to a directory of this test, not to tempdir(), the download cache of the
  # live tests.
  download_directory <- tempfile("live_string_downloads")
  dir.create(download_directory)
  run_live <- function(expression) {
    return(outcome_of(with_live_string(expression, urls = character(0), deactivate = signal_deactivated)))
  }

  # A body that works gives its value, and its warnings go on.
  checkEquals(3, with_live_string(1 + 2, urls = character(0), deactivate = signal_deactivated))
  warned <- FALSE
  withCallingHandlers(
    with_live_string(warning("a warning of the test"), urls = character(0), deactivate = signal_deactivated),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  checkTrue(warned)

  # A STRINGdb object whose STRING cannot be reached in the middle of a test: an API request, and a
  # download of the package's downloadAbsentFile.
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- UNREACHABLE_ADDRESS
  outcome <- run_live(string_db$get_homologs_besthits("9606.ENSP00000269305"))
  checkEquals("deactivated", outcome$kind)
  checkTrue(grepl("^STRING could not be reached: ", outcome$message))

  outcome <- suppressWarnings(run_live(
    STRINGdb::downloadAbsentFile(paste(UNREACHABLE_ADDRESS, "/9606.protein.info.v12.5.txt.gz", sep = ""), oD = download_directory)
  ))
  checkEquals("deactivated", outcome$kind)

  # An answer with HTTP status 5xx deactivates; a download refused with HTTP status 4xx fails.
  outcome <- run_live(STRINGdb:::stop_on_server_error(fake_response(524L), "https://version-12-5.string-db.org/api/tsv/enrichment"))
  checkEquals("deactivated", outcome$kind)

  outcome <- suppressWarnings(run_live({
    warning("cannot open URL 'https://stringdb-downloads.org/download/x.txt.gz': HTTP status was '404 Not Found'")
    stop("cannot open URL 'https://stringdb-downloads.org/download/x.txt.gz'")
  }))
  checkEquals("error", outcome$kind)

  # Assertion failures and other errors fail the test with their own message.
  outcome <- run_live(stop("Test not TRUE\n"))
  checkEquals("error", outcome$kind)
  checkEquals("Test not TRUE\n", outcome$message)

  outcome <- run_live(stop("ERROR: STRING refused the request with HTTP status 400"))
  checkEquals("error", outcome$kind)

  # An error that the test catches itself does not deactivate it.
  outcome <- run_live(tryCatch(httr::GET(UNREACHABLE_ADDRESS), error = function(e) "caught"))
  checkEquals("value", outcome$kind)

  unlink(download_directory, recursive = TRUE)

}
