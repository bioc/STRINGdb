# These tests replace the request to string-db.org (fetch_version_table), so
# they run offline and do not depend on the STRING servers.

# Stand-in for the request to string-db.org that fails the test when it is made.
no_version_request <- function(method, protocol) {
  stop("string-db.org was asked for '", method, "' although no request was expected")
}

# Stand-in for string-db.org when it cannot be reached (e.g. an SSL error).
unreachable_string <- function(method, protocol) {
  NULL
}


test_known_versions_need_no_request <- function() {

  resolved <- STRINGdb:::resolve_string_version("12.5", "https", fetch_version_table = no_version_request)
  checkEquals("12.5", resolved$version)
  checkEquals("https://version-12-5.string-db.org", resolved$stable_url)

  # A version without a dot gets ".0", as before.
  resolved <- STRINGdb:::resolve_string_version("11", "https", fetch_version_table = no_version_request)
  checkEquals("11.0", resolved$version)
  checkEquals("https://version-11-0.string-db.org", resolved$stable_url)

  resolved <- STRINGdb:::resolve_string_version("11.0b", "https", fetch_version_table = no_version_request)
  checkEquals("https://version-11-0b.string-db.org", resolved$stable_url)

}


test_unspecified_version <- function() {

  # Online, STRING's current version is used, as before.
  current_version_12_5 <- function(method, protocol) {
    checkEquals("version", method)
    data.frame(version = "12.5", stable_url = "https://version-12-5.string-db.org", stringsAsFactors = FALSE)
  }
  output <- capture.output(
    resolved <- STRINGdb:::resolve_string_version(character(0), "https", fetch_version_table = current_version_12_5)
  )
  checkEquals("12.5", resolved$version)
  checkEquals("https://version-12-5.string-db.org", resolved$stable_url)
  checkTrue(any(grepl("You didn't specify a version", output)))

  # A current version released after this package takes its address from STRING's answer.
  current_version_13_0 <- function(method, protocol) {
    data.frame(version = "13.0", stable_url = "https://version-13-0.string-db.org", stringsAsFactors = FALSE)
  }
  capture.output(
    resolved <- STRINGdb:::resolve_string_version(character(0), "https", fetch_version_table = current_version_13_0)
  )
  checkEquals("13.0", resolved$version)
  checkEquals("https://version-13-0.string-db.org", resolved$stable_url)

  # When STRING cannot be reached, the newest known version is used, with a WARNING.
  known_versions <- STRINGdb:::KNOWN_STRING_VERSIONS
  output <- capture.output(
    resolved <- STRINGdb:::resolve_string_version(character(0), "https", fetch_version_table = unreachable_string)
  )
  checkEquals(known_versions$version[nrow(known_versions)], resolved$version)
  checkEquals(known_versions$stable_url[nrow(known_versions)], resolved$stable_url)
  checkTrue(any(grepl("could not be reached", output)))

}


test_unknown_version <- function() {

  available_versions <- function(method, protocol) {
    checkEquals("available_api_versions", method)
    data.frame(
      version = c("12.5", "13.0"),
      stable_url = c("https://version-12-5.string-db.org", "https://version-13-0.string-db.org"),
      stringsAsFactors = FALSE
    )
  }

  # A version that the package does not know is looked up on STRING.
  resolved <- STRINGdb:::resolve_string_version("13.0", "https", fetch_version_table = available_versions)
  checkEquals("13.0", resolved$version)
  checkEquals("https://version-13-0.string-db.org", resolved$stable_url)

  # A version that STRING does not serve stops, and the error names the available versions.
  error_message <- tryCatch(
    {
      STRINGdb:::resolve_string_version("10", "https", fetch_version_table = available_versions)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  checkTrue(!is.null(error_message))
  checkTrue(grepl("version 10.0 is not valid", error_message, fixed = TRUE))
  checkTrue(grepl("12.5, 13.0", error_message, fixed = TRUE))

  # An unknown version cannot be checked when STRING cannot be reached.
  error_message <- tryCatch(
    {
      STRINGdb:::resolve_string_version("13.0", "https", fetch_version_table = unreachable_string)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  checkTrue(!is.null(error_message))
  checkTrue(grepl("could not be reached", error_message, fixed = TRUE))

}


test_parse_string_version_table <- function() {

  parsed <- STRINGdb:::parse_string_version_table(
    "11.0\thttps://version-11-0.string-db.org\n11.0b\thttps://version-11-0b.string-db.org\n12.5\thttps://version-12-5.string-db.org\n"
  )
  checkEquals(c("11.0", "11.0b", "12.5"), parsed$version)
  checkEquals("https://version-12-5.string-db.org", parsed$stable_url[3])

  # Answers that are not version tables, such as an error page, are not read as one.
  checkTrue(is.null(STRINGdb:::parse_string_version_table("<html><body>Just a moment...</body></html>")))
  checkTrue(is.null(STRINGdb:::parse_string_version_table("")))
  checkTrue(is.null(STRINGdb:::parse_string_version_table(NULL)))

}


test_constructor_with_known_version <- function() {

  # A known version needs no request to STRING when the object is created.
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  checkEquals("12.5", string_db$version)
  checkEquals("12.5", string_db$file_version)
  checkEquals("https://version-12-5.string-db.org", string_db$stable_url)

  string_db <- STRINGdb$new(version = "11.0b", species = 9606, score_threshold = 400, protocol = "http")
  checkEquals("11.0", string_db$file_version)
  checkEquals("http://version-11-0b.string-db.org", string_db$stable_url)

}
