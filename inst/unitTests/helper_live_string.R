# Helpers for the unit tests that call STRING: its API (string-db.org) or its download server
# (stringdb-downloads.org). The Bioconductor build machines sometimes cannot reach STRING (the requests
# failed, for example, with SSL connect errors). A test that cannot reach STRING is then deactivated with
# RUnit's DEACTIVATED(), which BiocGenerics:::testPackage() reports without failing the suite, instead of
# failing. Assertion failures and all other errors still fail the test.
#
# The name of this file does not start with "test_", so RUnit does not run it as a test file. A test file
# with tests that call STRING sources it:
#
#   source(system.file("unitTests", "helper_live_string.R", package = "STRINGdb"), local = TRUE)
#
# and wraps the body of each such test function in with_live_string():
#
#   test_get_enrichment <- function() {
#     with_live_string({
#       ...
#     })
#   }
#
# None of the names below starts with "test", so RUnit does not run them as test functions.
#
# RUnit prints the stop() of DEACTIVATED() like an error, so the protocol of a deactivated test (for
# example tests/runTests.Rout) shows a line such as
#   Error in deactivate(paste("STRING could not be reached: ", ...)) : STRING could not be reached: ...
# The test is nevertheless counted as deactivated, not as an error.


# The addresses that are asked before a test whether STRING can be reached: STRING's API at the stable
# address of the version that the tests use, and the server of the download files.
LIVE_STRING_URLS <- c(
  "https://version-12-5.string-db.org/api/json/version",
  "https://stringdb-downloads.org/"
)

# Seconds to wait for the answer of each of these addresses.
LIVE_STRING_TIMEOUT <- 10


# Errors of curl (the requests of httr) that are due to the request itself, not to the connection to
# STRING. curl_error is the class of the curl errors since version 5 of the curl package.
REQUEST_CURL_ERROR_CLASSES <- c(
  "curl_error_url_malformat",
  "curl_error_unsupported_protocol",
  "curl_error_bad_function_argument"
)

# Messages of connection-level failures (besides SSL errors), in any case: of curl, whose errors had no
# class before version 5 of the curl package, and of download.file(), which tells why it could not
# download a file in a warning.
CONNECTION_FAILURE_PATTERN <- paste(
  "timeout was reached", "timed out",
  "could not resolve", "couldn't resolve",
  "could not connect", "couldn't connect", "failed to connect",
  "connection refused", "connection reset", "network is unreachable", "no route to host",
  "recv failure", "send failure", "failure when receiving data", "empty reply from server",
  "server returned nothing", "HTTP/2 stream",
  sep = "|"
)


# A message on one line (curl's messages have several), for the protocol of the tests.
single_line_message <- function(text) {
  return(trimws(gsub("[[:space:]]+", " ", text)))
}


# Asks each address of urls, waiting at most timeout_seconds for each. Returns NULL when all of them
# answered with an HTTP status below 500 other than 403, or else why one of them could not be reached: the
# error of the request (an SSL, connection, timeout or DNS error) or the HTTP status of its answer, 5xx or
# 403. These addresses are open to everyone, so a 403 means that STRING does not serve this machine, for
# example a block or a "Just a moment..." challenge page of the Cloudflare protection of STRING's API;
# the tests would otherwise read that page as if it were STRING's answer.
string_unreachable_reason <- function(urls = LIVE_STRING_URLS, timeout_seconds = LIVE_STRING_TIMEOUT) {
  for (probe_url in urls) {
    response <- tryCatch(
      httr::GET(probe_url, httr::timeout(timeout_seconds), query = list(caller_identity = "STRINGdb-package")),
      error = function(e) e
    )

    if (inherits(response, "error")) {
      return(paste(probe_url, ": ", single_line_message(conditionMessage(response)), sep = ""))
    }

    status <- httr::status_code(response)
    if (status >= 500 || status == 403) {
      return(paste(probe_url, " answered with HTTP status ", status, sep = ""))
    }
  }

  return(NULL)
}


# Deactivates the calling test when one of urls cannot be reached (string_unreachable_reason); urls =
# character(0) asks none. deactivate is RUnit's DEACTIVATED, which stops the test; it is an argument so
# that the tests of this helper can replace it.
skip_unless_string_reachable <- function(urls = LIVE_STRING_URLS, timeout_seconds = LIVE_STRING_TIMEOUT,
                                         deactivate = RUnit::DEACTIVATED) {
  reason <- string_unreachable_reason(urls, timeout_seconds)
  if (!is.null(reason)) {
    deactivate(paste("STRING could not be reached: ", reason, sep = ""))
  }
  return(invisible(NULL))
}


# TRUE when error (a condition) means that STRING could not be reached, rather than a fault of the
# package or of the test:
#   - the package's own error for an answer with HTTP status 5xx (stop_on_server_error)
#   - a curl error of the connection: SSL, connection, timeout or DNS
#   - a failed download: download.file() stops with "cannot open URL '...'" (or "download from '...'
#     failed") and tells why in a warning (warning_messages). A file that STRING refused with an HTTP
#     status 4xx is not a connection failure: it points at a wrong address.
# Assertion failures ("Test not TRUE"), STRING's refusals of a request (HTTP status 4xx) and all other
# errors give FALSE.
is_string_connection_error <- function(error, warning_messages = character(0)) {
  message <- conditionMessage(error)

  if (grepl("STRING request failed with HTTP status 5[0-9][0-9]", message)) {
    return(TRUE)
  }

  if (inherits(error, "curl_error")) {
    return(!inherits(error, REQUEST_CURL_ERROR_CLASSES))
  }

  if (grepl("SSL", message, fixed = TRUE) || grepl(CONNECTION_FAILURE_PATTERN, message, ignore.case = TRUE)) {
    return(TRUE)
  }

  failed_download <- grepl("cannot open URL|download from '.*' failed", message)
  if (failed_download) {
    refused_download <- any(grepl("HTTP status was '4[0-9][0-9]", c(message, warning_messages)))
    return(!refused_download)
  }

  return(FALSE)
}


# Evaluates expr, the body of a test that calls STRING, and returns its value. The test is deactivated
# (deactivate, RUnit's DEACTIVATED) when STRING cannot be reached: before expr is evaluated, when one of
# urls does not answer (skip_unless_string_reachable; urls = character(0) asks none), and when expr stops
# with a connection-level error (is_string_connection_error). Assertion failures and all other errors go
# on as they are, so that the test fails; warnings go on as well.
# A download that is cut off part-way leaves nothing in the download cache (downloadAbsentFile), so a later
# test of the same R session downloads the whole file again.
with_live_string <- function(expr, urls = LIVE_STRING_URLS, deactivate = RUnit::DEACTIVATED) {
  skip_unless_string_reachable(urls, deactivate = deactivate)

  # download.file() tells why a download failed only in a warning, so the warnings of expr are noted
  # (and not muffled)
  warning_messages <- character(0)

  # calling handlers see a condition before the stack is unwound: an error that is not a connection
  # failure is not handled here and fails the test as it is. An error that the test catches itself
  # (tryCatch) does not reach these handlers.
  withCallingHandlers(
    expr,
    warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
    },
    error = function(e) {
      if (is_string_connection_error(e, warning_messages)) {
        deactivate(paste("STRING could not be reached: ", single_line_message(conditionMessage(e)), sep = ""))
      }
    }
  )
}
