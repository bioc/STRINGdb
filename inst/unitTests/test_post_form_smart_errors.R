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


test_string_api_error_message <- function() {

  # The answer of /api/image/network for a PNG of too many interactions.
  png_limit_body <- paste(
    '{ "Error" : "Too many interactions for a PNG", "ErrorMessage" : "This network has 179,123 interactions,',
    'more than the 150,000 that a PNG can be drawn for. Ask for the picture as SVG (/api/svg/network),',
    'or for fewer interactions: a higher required_score (e.g. 700 or 900), or fewer proteins." }'
  )
  explanation <- STRINGdb:::string_api_error_message(png_limit_body)
  checkTrue(grepl("^Too many interactions for a PNG: This network has 179,123 interactions", explanation))
  checkTrue(grepl("a higher required_score", explanation, fixed = TRUE))

  # HTML in the message and quotes written as &quot;; STRING's generic title 'Error' is left out.
  html_body <- '{ "Error" : "Error", "ErrorMessage" : "<p>STRING did not find &quot;xyz&quot;.</p><p>Try<br/>again.</p>" }'
  checkEquals(STRINGdb:::string_api_error_message(html_body), 'STRING did not find "xyz". Try again.')

  # Bodies without STRING's fields.
  checkTrue(is.null(STRINGdb:::string_api_error_message("<html>Bad gateway</html>")))
  checkTrue(is.null(STRINGdb:::string_api_error_message(NULL)))

}


test_stop_on_client_error <- function() {

  # Minimal stand-in for an httr response with a body (httr keeps the body as raw bytes).
  fake_response <- function(status, body) {
    structure(list(status_code = status, content = charToRaw(body)), class = "response")
  }
  uri <- "https://string-db.org/api/svg/network"
  refused_body <- '{ "Error" : "No proteins found", "ErrorMessage" : "No proteins of Homo sapiens are annotated with GO:9999999." }'

  # A refused picture must stop with STRING's explanation instead of being read as a PNG or returned as the SVG.
  error_message <- tryCatch(
    {
      STRINGdb:::stop_on_client_error(fake_response(404L, refused_body), uri)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  checkTrue(!is.null(error_message))
  checkTrue(grepl("HTTP status 404", error_message))
  checkTrue(grepl("No proteins found: No proteins of Homo sapiens are annotated with GO:9999999.", error_message, fixed = TRUE))

  # A refusal without STRING's explanation stops too.
  error_message <- tryCatch(
    {
      STRINGdb:::stop_on_client_error(fake_response(400L, "Bad request"), uri)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  checkTrue(!is.null(error_message))
  checkTrue(grepl("HTTP status 400", error_message))

  # Successful answers pass; server errors are left to stop_on_server_error.
  checkTrue(!is.null(STRINGdb:::stop_on_client_error(fake_response(200L, "<svg></svg>"), uri)))
  checkTrue(!is.null(STRINGdb:::stop_on_client_error(fake_response(524L, "Gateway timeout"), uri)))

}
