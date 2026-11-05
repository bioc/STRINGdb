
# The package's caller identity: the request helpers send it with every request to STRING, and no
# method passes it on. The four methods that had a caller_identity argument in a release keep it,
# ignored, until the next release.


# Evaluates request with httr's POST and GET traced as the package calls them: the tracers keep the
# address and the parameters of the request and stop it before anything is sent. Returns them
# (NULL when no request was made).
capture_string_request <- function(request) {
  captured <- new.env()
  suppressMessages(trace("POST", where = asNamespace("STRINGdb"), print = FALSE,
    tracer = bquote({
      assign("url", url, envir = .(captured))
      assign("parameters", body, envir = .(captured))
      stop("captured by the test")
    })))
  on.exit(suppressMessages(untrace("POST", where = asNamespace("STRINGdb"))), add = TRUE)
  suppressMessages(trace("GET", where = asNamespace("STRINGdb"), print = FALSE,
    tracer = bquote({
      assign("url", url, envir = .(captured))
      assign("parameters", list(...)$query, envir = .(captured))
      stop("captured by the test")
    })))
  on.exit(suppressMessages(untrace("GET", where = asNamespace("STRINGdb"))), add = TRUE)

  tryCatch(request, error = function(e) NULL)
  return(list(url = captured$url, parameters = captured$parameters))
}


test_requests_send_caller_identity <- function() {

  checkEquals(list(caller_identity = "STRINGdb-package"), STRINGdb:::with_caller_identity(NULL))
  checkEquals(list(a = "1", caller_identity = "STRINGdb-package"), STRINGdb:::with_caller_identity(list(a = "1")))
  checkEquals(list(caller_identity = "STRINGdb-package"), STRINGdb:::with_caller_identity(list(caller_identity = "other")))

  # the request helpers
  sent <- capture_string_request(STRINGdb:::postFormSmart("https://example.org/api", list(a = "1")))
  checkEquals(list(a = "1", caller_identity = "STRINGdb-package"), sent$parameters)
  sent <- capture_string_request(STRINGdb:::post_string_json("https://example.org/api", list(a = "1")))
  checkEquals(list(a = "1", caller_identity = "STRINGdb-package"), sent$parameters)
  sent <- capture_string_request(STRINGdb:::get_string_text("https://example.org/download?networkId=n1", list(format = "tsv")))
  checkEquals("https://example.org/download?networkId=n1", sent$url)
  checkEquals(list(format = "tsv", caller_identity = "STRINGdb-package"), sent$parameters)
  sent <- capture_string_request(STRINGdb:::fetch_string_version_table("version"))
  checkEquals(list(caller_identity = "STRINGdb-package"), sent$parameters)

  # the methods send it without handling it: get_enrichment through postFormSmart, and
  # get_enrichment_figure with its own POST
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"
  sent <- capture_string_request(string_db$get_enrichment("9606.ENSP00000269305"))
  checkEquals("STRINGdb-package", sent$parameters$caller_identity)
  sent <- capture_string_request(string_db$get_enrichment_figure("9606.ENSP00000269305"))
  checkEquals("STRINGdb-package", sent$parameters$caller_identity)

}


test_downloads_send_caller_identity <- function() {

  # STRING's addresses get the identity after their own parameters; other addresses stay as they are
  checkEquals("https://stringdb-downloads.org/download/a.txt.gz?caller_identity=STRINGdb-package",
              STRINGdb:::url_with_caller_identity("https://stringdb-downloads.org/download/a.txt.gz"))
  checkEquals("https://version-12-5.string-db.org/api/tsv/x?a=1&caller_identity=STRINGdb-package",
              STRINGdb:::url_with_caller_identity("https://version-12-5.string-db.org/api/tsv/x?a=1"))
  checkEquals("https://example.org/a.txt.gz", STRINGdb:::url_with_caller_identity("https://example.org/a.txt.gz"))
  checkEquals("https://notstring-db.org.example.com/a", STRINGdb:::url_with_caller_identity("https://notstring-db.org.example.com/a"))
  checkEquals("file:///tmp/string-db.org/a.txt.gz", STRINGdb:::url_with_caller_identity("file:///tmp/string-db.org/a.txt.gz"))

  # downloadAbsentFile asks for STRING's file with the identity, in binary mode, and saves it under
  # the name of the address without it: download.file is traced, and the tracer stops before
  # anything is downloaded
  captured <- new.env()
  suppressMessages(trace("download.file", where = asNamespace("STRINGdb"), print = FALSE,
    tracer = bquote({
      assign("url", url, envir = .(captured))
      assign("destfile", destfile, envir = .(captured))
      assign("mode", mode, envir = .(captured))
      stop("captured by the test")
    })))
  on.exit(suppressMessages(untrace("download.file", where = asNamespace("STRINGdb"))), add = TRUE)
  cache <- tempfile("download_cache")
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  address <- "https://stringdb-downloads.org/download/protein.info.v12.5/9606.protein.info.v12.5.txt.gz"
  tryCatch(STRINGdb::downloadAbsentFile(address, oD = cache), error = function(e) NULL)
  checkEquals(paste(address, "?caller_identity=STRINGdb-package", sep = ""), captured$url)
  checkEquals("wb", captured$mode)
  checkTrue(startsWith(basename(captured$destfile), "9606.protein.info.v12.5.txt.gz."))

}


test_caller_identity_argument_is_ignored <- function() {

  # only the four methods that had a caller_identity argument in a release keep it, with the
  # default NULL, until the next release; the other methods take none
  methods_env <- STRINGdb$def@refMethods
  method_names <- Filter(function(name) is.function(get(name, envir = methods_env)), ls(methods_env))
  with_argument <- Filter(function(name) "caller_identity" %in% names(formals(get(name, envir = methods_env))), method_names)
  checkEquals(c("get_enrichment_figure", "get_geneset_description", "get_link", "get_png"), sort(with_argument))
  for (name in with_argument) {
    checkTrue(is.null(formals(get(name, envir = methods_env))$caller_identity))
  }

  # a call that gives one still runs, with a warning once per R session, and STRING gets the
  # package's caller identity; a call without one gets no warning
  warning_state <- STRINGdb:::caller_identity_warning
  assign("given", FALSE, envir = warning_state)
  on.exit(assign("given", FALSE, envir = warning_state), add = TRUE)
  warned <- character(0)
  remember_warning <- function(w) {
    warned <<- c(warned, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  identity_warnings <- function() {
    return(warned[grepl("caller_identity", warned, fixed = TRUE)])
  }

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"
  withCallingHandlers(capture_string_request(string_db$get_png("9606.ENSP00000269305")), warning = remember_warning)
  checkEquals(0, length(identity_warnings()))

  sent <- withCallingHandlers(
    capture_string_request(string_db$get_geneset_description("9606.ENSP00000269305", caller_identity = "my-tool")),
    warning = remember_warning
  )
  checkEquals("STRINGdb-package", sent$parameters$caller_identity)
  checkEquals(1, length(identity_warnings()))
  checkTrue(grepl("is ignored", identity_warnings(), fixed = TRUE))
  checkTrue(grepl("removed in the next Bioconductor release, so do not specify it", identity_warnings(), fixed = TRUE))

  sent <- withCallingHandlers(
    capture_string_request(string_db$get_png("9606.ENSP00000269305", caller_identity = "my-tool")),
    warning = remember_warning
  )
  checkEquals("STRINGdb-package", sent$parameters$caller_identity)
  checkEquals(1, length(identity_warnings()))

}
