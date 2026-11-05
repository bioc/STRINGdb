# Offline tests of STRING's values/ranks enrichment (get_values_ranks_enrichment, get_values_ranks_results,
# get_api_key). The requests to STRING are replaced by stand-ins (send, fetch), and so are
# the waits (sleep, clock), so that these tests need neither a network nor an API key.

# A stand-in for STRING's job methods: answers each request with the next record of
# 'answers' and keeps the requests in the environment 'log'.
fake_string_json <- function(answers, log = new.env()) {
  log$requests <- list()
  log$next_answer <- 1
  function(url_str, params, timeout_seconds = 60) {
    log$requests[[length(log$requests) + 1]] <- list(url = url_str, params = params)
    answer <- answers[[min(log$next_answer, length(answers))]]
    log$next_answer <- log$next_answer + 1
    if (inherits(answer, "error")) stop(answer)
    return(answer)
  }
}

status_record <- function(status, message = "", download_url = "") {
  list(list(
    job_id = "job123", creation_time = "2026-11-04 12:00:00", string_version = "12.5",
    status = status, message = message, page_url = if (nchar(download_url) > 0) "https://example.org/page" else "",
    download_url = download_url, graph_url = if (nchar(download_url) > 0) "https://example.org/figure" else ""
  ))
}

# A clock that advances by the time slept, and a sleep that only records the waits.
fake_waits <- function() {
  waits <- new.env()
  waits$now <- 0
  waits$intervals <- numeric(0)
  waits$sleep <- function(seconds) {
    waits$intervals <- c(waits$intervals, seconds)
    waits$now <- waits$now + seconds
  }
  waits$clock <- function() waits$now
  return(waits)
}

# Evaluates expression with the package's key file in a temporary directory (R_USER_CONFIG_DIR,
# which tools::R_user_dir reads) and without STRINGDB_API_KEY, so that no test reads or writes the
# API key of this computer. The environment is restored afterwards. Returns the value of
# expression.
with_temporary_key_file <- function(expression) {
  config_directory <- tempfile("stringdb_config")
  previous_values <- Sys.getenv(c("R_USER_CONFIG_DIR", "STRINGDB_API_KEY"), unset = NA)
  on.exit(
    {
      for (variable_name in names(previous_values)) {
        if (is.na(previous_values[[variable_name]])) {
          Sys.unsetenv(variable_name)
        } else {
          do.call(Sys.setenv, stats::setNames(list(previous_values[[variable_name]]), variable_name))
        }
      }
      unlink(config_directory, recursive = TRUE)
    },
    add = TRUE
  )
  Sys.setenv(R_USER_CONFIG_DIR = config_directory)
  Sys.unsetenv("STRINGDB_API_KEY")
  return(force(expression))
}

values_ranks_tsv <- paste(
  "category\ttermID\ttermDescription\tgenesMapped\tgenesInSet\tenrichmentScore\tdirection\tfalseDiscoveryRate\tmethod\tproteinIDs\tproteinLabels\tproteinInputLabels\tproteinInputValues\tproteinRanks",
  "GO Process\tGO:0006281\tDNA repair # and more\t3\t517\t1.25\ttop\t0.0012\tks\t9606.A,9606.B,9606.C\tA,B,C\ta,b,c\t2.1,1.9,1.5\t1,2,3",
  "KEGG\thsa04110\tCell cycle\t2\t157\t-0.8\tbottom\t0.03\tafc\t9606.D,9606.E\tD,E\td,e\t-1.2,-2.2\t5,6",
  sep = "\n"
)


test_values_ranks_input_preparation <- function() {

  input <- STRINGdb:::prepare_values_ranks_input(
    c("9606.A", "9606.B", NA, "", "9606.A", "9606.C", "9606.D"),
    c(1.5, -2, 3, 4, -2.5, NA, Inf)
  )
  # NA identifier, empty identifier, NA value and infinite value are left out; 9606.A keeps -2.5
  checkEquals(4, input$n_missing)
  checkEquals(1, input$n_duplicates)
  checkEquals(2, input$n_submitted)
  checkEquals("9606.B\t-2\n9606.A\t-2.5", input$text)

  checkException(STRINGdb:::prepare_values_ranks_input(c("9606.A", "9606.B"), 1), silent = TRUE)
  checkException(STRINGdb:::prepare_values_ranks_input("9606.A", "1.5"), silent = TRUE)
  checkException(STRINGdb:::prepare_values_ranks_input(c("9606.A", NA), c(NA, 1)), silent = TRUE)
  checkException(STRINGdb:::prepare_values_ranks_input("9606.A\tx", 1), silent = TRUE)

}


test_values_ranks_options <- function() {

  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
  checkEquals("0.01", options$ge_fdr)
  checkEquals("-1", options$ge_enrichment_rank_direction)
  checkEquals("1", STRINGdb:::normalize_values_ranks_options(0.25, "ascending")$ge_enrichment_rank_direction)

  # above 0.25 the computation loses its optimizations, so it is refused
  checkException(STRINGdb:::normalize_values_ranks_options(0.3, "descending"), silent = TRUE)
  checkException(STRINGdb:::normalize_values_ranks_options(0, "descending"), silent = TRUE)
  checkException(STRINGdb:::normalize_values_ranks_options(NA_real_, "descending"), silent = TRUE)
  checkException(STRINGdb:::normalize_values_ranks_options(0.01, "top"), silent = TRUE)

  # a key given as the argument is used as it is, without surrounding spaces
  error_message <- tryCatch(STRINGdb:::resolve_values_ranks_api_key(""), error = function(e) conditionMessage(e))
  checkTrue(grepl("api_key should be one STRING API key", error_message, fixed = TRUE))
  checkEquals(list(key = "abc", source = "argument"), STRINGdb:::resolve_values_ranks_api_key(" abc "))

}


test_values_ranks_submit <- function() {

  log <- new.env()
  send <- fake_string_json(list(list(list(job_id = "job123", status = "submitted", message = "Job was successfully submitted to the queue!"))), log)
  options <- STRINGdb:::normalize_values_ranks_options(0.05, "descending")

  job_id <- STRINGdb:::submit_values_ranks_job("https://version-12-5.string-db.org", 9606, "9606.A\t1.5", "key1", options, send = send)
  checkEquals("job123", job_id)

  request <- log$requests[[1]]
  checkEquals("https://version-12-5.string-db.org/api/json/valuesranks_enrichment_submit", request$url)
  checkEquals("key1", request$params$api_key)
  checkEquals("9606", request$params$species)
  checkEquals("9606.A\t1.5", request$params$identifiers)
  checkEquals("STRINGdb-package", request$params$caller_identity)
  checkEquals("0.05", request$params$ge_fdr)
  checkEquals("-1", request$params$ge_enrichment_rank_direction)

  # a large taxon identifier is not written in scientific notation
  log2 <- new.env()
  STRINGdb:::submit_values_ranks_job("u", 300000, "x\t1", "key1", options, send = fake_string_json(list(list(list(job_id = "j", status = "submitted"))), log2))
  checkEquals("300000", log2$requests[[1]]$params$species)

  # STRING answers errors with a record whose status is "error"
  pending <- fake_string_json(list(list(list(job_id = "", status = "error", message = "Authorizing the api key. Please be patient."))))
  error_message <- tryCatch(STRINGdb:::submit_values_ranks_job("u", 9606, "x\t1", "key1", options, send = pending), error = function(e) conditionMessage(e))
  checkTrue(grepl("still activating this API key", error_message, fixed = TRUE))

  limit <- fake_string_json(list(list(list(job_id = "", status = "error", message = "Job submission limit reached. Please wait for your jobs to finish before submitting new ones."))))
  error_message <- tryCatch(STRINGdb:::submit_values_ranks_job("u", 9606, "x\t1", "key1", options, send = limit), error = function(e) conditionMessage(e))
  checkTrue(grepl("Job submission limit reached", error_message, fixed = TRUE))

}


test_values_ranks_wait <- function() {

  # submitted, then running, then finished: the waits grow from 5 s
  waits <- fake_waits()
  send <- fake_string_json(list(
    status_record("submitted"),
    status_record("running", "computing statistical enrichment p-values ..."),
    status_record("running", "computing statistical enrichment p-values ..."),
    status_record("success", "Job finished", "https://example.org/download")
  ))
  job <- STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = waits$sleep, clock = waits$clock)
  checkEquals("success", job$status)
  checkEquals("https://example.org/download", job$download_url)
  checkEquals(c(5, 7.5, 11.25), waits$intervals)

  # STRING's 'failure.' is a failure, with its message
  send <- fake_string_json(list(status_record("failure.", "Too many submissions (> 50) by your IP number in the STRING queue")))
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = fake_waits()$sleep),
    error = function(e) conditionMessage(e))
  checkTrue(grepl("Too many submissions", error_message, fixed = TRUE))

  send <- fake_string_json(list(status_record("expired")))
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = fake_waits()$sleep),
    error = function(e) conditionMessage(e))
  checkTrue(grepl("no longer has", error_message, fixed = TRUE))

  # a job that does not finish in time: the error says how to get its results later
  waits <- fake_waits()
  send <- fake_string_json(list(status_record("running")))
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 60, quiet = TRUE, send = send, sleep = waits$sleep, clock = waits$clock),
    error = function(e) conditionMessage(e))
  checkTrue(grepl("did not finish within 60 seconds", error_message, fixed = TRUE))
  checkTrue(grepl("get_values_ranks_results(\"job123\")", error_message, fixed = TRUE))

  # a failed status request is tried again; five in a row give up
  waits <- fake_waits()
  send <- fake_string_json(list(simpleError("SSL connect error"), status_record("success", "Job finished", "https://example.org/download")))
  job <- STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = waits$sleep, clock = waits$clock)
  checkEquals("success", job$status)

  send <- fake_string_json(list(simpleError("SSL connect error")))
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = fake_waits()$sleep),
    error = function(e) conditionMessage(e))
  checkTrue(grepl("could not be read", error_message, fixed = TRUE))
  checkTrue(grepl("get_values_ranks_results(\"job123\")", error_message, fixed = TRUE))

  # the status request is a POST with the key and the job, not a URL with the key
  log <- new.env()
  send <- fake_string_json(list(status_record("success", "Job finished", "https://example.org/download")), log)
  STRINGdb:::wait_for_values_ranks_job("https://version-12-5.string-db.org", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = fake_waits()$sleep)
  checkEquals("https://version-12-5.string-db.org/api/json/valuesranks_enrichment_status", log$requests[[1]]$url)
  checkEquals(list(api_key = "key1", job_id = "job123", caller_identity = "STRINGdb-package"), log$requests[[1]]$params)

  # a refused status request (a job of another key, an unknown key) stops at once: asking
  # again cannot help
  log <- new.env()
  send <- fake_string_json(list(list(list(job_id = "", status = "error", message = "Job does not exist. Please use a valid id or submit a new job."))), log)
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "key2", "job123", 7200, quiet = TRUE, send = send, sleep = fake_waits()$sleep),
    error = function(e) conditionMessage(e))
  checkEquals(1, length(log$requests))
  checkTrue(grepl("Job does not exist", error_message, fixed = TRUE))
  checkTrue(grepl("keeps each job for the API key that submitted it", error_message, fixed = TRUE))

  log <- new.env()
  send <- fake_string_json(list(list(list(job_id = "", status = "error", message = "Api key does not exist. Please use a valid key or generate a new one."))), log)
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "bad", "job123", 7200, quiet = TRUE, send = send, sleep = fake_waits()$sleep),
    error = function(e) conditionMessage(e))
  checkEquals(1, length(log$requests))
  # the key that submitted the job was accepted, so this is another key: a new key cannot help
  checkTrue(grepl("Api key does not exist", error_message, fixed = TRUE))
  checkTrue(grepl("keeps each job for the API key that submitted it", error_message, fixed = TRUE))
  checkTrue(!grepl("get_api_key", error_message, fixed = TRUE))

  # an empty answer (STRING sent no record for the job) stops at once as well
  log <- new.env()
  send <- fake_string_json(list(list()), log)
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = fake_waits()$sleep),
    error = function(e) conditionMessage(e))
  checkEquals(1, length(log$requests))
  checkTrue(grepl("gave no state for values/ranks job job123", error_message, fixed = TRUE))

  # a job that failed on STRING (status "error" with its job_id) failed; its request was not refused
  send <- fake_string_json(list(status_record("error", "The enrichment analysis could not be computed. Please try again later.")))
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = fake_waits()$sleep),
    error = function(e) conditionMessage(e))
  checkTrue(grepl("job123 failed: The enrichment analysis could not be computed", error_message, fixed = TRUE))
  checkTrue(!grepl("refused", error_message, fixed = TRUE))

  # the wait classifies a job by the first word of its status, so this one stops at once
  waits <- fake_waits()
  send <- fake_string_json(list(status_record("Error. Something went wrong")))
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = waits$sleep, clock = waits$clock),
    error = function(e) conditionMessage(e))
  checkTrue(grepl("job123 failed", error_message, fixed = TRUE))
  checkEquals(0, length(waits$intervals))

  # when curl passes an interrupt during a status request on as an error (aborted by a callback),
  # the wait ends at once
  log <- new.env()
  send <- fake_string_json(list(simpleError("Operation was aborted by an application callback")), log)
  error_message <- tryCatch(STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = TRUE, send = send, sleep = fake_waits()$sleep),
    error = function(e) conditionMessage(e))
  checkEquals(1, length(log$requests))
  checkTrue(grepl("was interrupted", error_message, fixed = TRUE))
  checkTrue(grepl("get_values_ranks_results(\"job123\")", error_message, fixed = TRUE))

}


test_values_ranks_wait_progress <- function() {

  # the state is the first word of STRING's status, without dots
  checkEquals("failure", STRINGdb:::values_ranks_state("failure."))
  checkEquals("error", STRINGdb:::values_ranks_state(" Error. Something went wrong"))
  checkEquals("", STRINGdb:::values_ranks_state(""))

  # a progress message only when the state or its message changes, not for a new number in it
  send <- fake_string_json(list(
    status_record("submitted", "Your job is number 3 in the queue"),
    status_record("submitted", "Your job is number 2 in the queue"),
    status_record("running", "computing statistical enrichment p-values ..."),
    status_record("running", "computing statistical enrichment p-values ..."),
    status_record("success", "Job finished", "https://example.org/download")
  ))
  progress <- character(0)
  withCallingHandlers(
    STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, quiet = FALSE, send = send, sleep = fake_waits()$sleep),
    message = function(m) {
      progress <<- c(progress, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  checkEquals(2, length(progress))
  checkTrue(grepl("number 3 in the queue", progress[1], fixed = TRUE))
  checkTrue(grepl("running", progress[2], fixed = TRUE))

}


test_values_ranks_results <- function() {

  results <- STRINGdb:::read_values_ranks_results(values_ranks_tsv, "job123")
  checkEquals(STRINGdb:::VALUES_RANKS_RESULT_COLUMNS, colnames(results))
  checkEquals(2, nrow(results))
  checkEquals("DNA repair # and more", results$termDescription[1])
  checkEquals(c(3L, 2L), results$genesMapped)
  checkEquals(c(517L, 157L), results$genesInSet)
  checkEquals(c(1.25, -0.8), results$enrichmentScore)
  checkEquals(c(0.0012, 0.03), results$falseDiscoveryRate)
  checkEquals(c("top", "bottom"), results$direction)

  # the text columns stay text, also a label that reads as a number or as NA
  numeric_looking <- paste(
    strsplit(values_ranks_tsv, "\n")[[1]][1],
    "KEGG\thsa04110\tNA\t2\t157\t-0.8\tbottom\t0.03\tafc\t9606.D,9606.E\t1\tNA\t-1.2,-2.2\t5,6",
    sep = "\n"
  )
  results <- STRINGdb:::read_values_ranks_results(numeric_looking, "job123")
  checkEquals("NA", results$termDescription)
  checkEquals("1", results$proteinLabels)
  checkEquals("NA", results$proteinInputLabels)

  # a table without terms has typed columns and no rows
  empty <- STRINGdb:::read_values_ranks_results(strsplit(values_ranks_tsv, "\n")[[1]][1], "job123")
  checkEquals(STRINGdb:::VALUES_RANKS_RESULT_COLUMNS, colnames(empty))
  checkEquals(0, nrow(empty))
  checkTrue(is.integer(empty$genesMapped))
  checkTrue(is.numeric(empty$falseDiscoveryRate))

  # an empty answer is STRING's answer for results it no longer has
  error_message <- tryCatch(STRINGdb:::read_values_ranks_results("", "job123"), error = function(e) conditionMessage(e))
  checkTrue(grepl("no results for values/ranks job job123", error_message, fixed = TRUE))
  error_message <- tryCatch(STRINGdb:::read_values_ranks_results("<html>Gateway</html>", "job123"), error = function(e) conditionMessage(e))
  checkTrue(grepl("is not a results table", error_message, fixed = TRUE))

  # waiting, downloading and the attributes of the results
  send <- fake_string_json(list(status_record("success", "Job finished", "https://example.org/download")))
  fetched <- new.env()
  fetch <- function(url_str, query = NULL, timeout_seconds = 300) {
    fetched$url <- url_str
    fetched$query <- query
    return(values_ranks_tsv)
  }
  results <- STRINGdb:::collect_values_ranks_results("u", "key1", "job123", 7200, quiet = TRUE, send = send, fetch = fetch, sleep = fake_waits()$sleep)
  checkEquals("https://example.org/download", fetched$url)
  checkEquals(list(caller_identity = "STRINGdb-package"), fetched$query)
  checkEquals(2, nrow(results))
  checkEquals("job123", attr(results, "job_id"))
  checkEquals("https://example.org/page", attr(results, "page_url"))
  checkEquals("https://example.org/figure", attr(results, "graph_url"))
  checkEquals("12.5", attr(results, "string_version"))

  # a failed download says how to get the results later
  send <- fake_string_json(list(status_record("success", "Job finished", "https://example.org/download")))
  refused <- function(url_str, query = NULL, timeout_seconds = 300) {
    stop("ERROR: STRING refused the request with HTTP status 400 for https://example.org/download: Session Expired", call. = FALSE)
  }
  error_message <- tryCatch(STRINGdb:::collect_values_ranks_results("u", "key1", "job123", 7200, quiet = TRUE, send = send, fetch = refused, sleep = fake_waits()$sleep),
    error = function(e) conditionMessage(e))
  checkTrue(grepl("results of values/ranks job job123 could not be downloaded", error_message, fixed = TRUE))
  checkTrue(grepl("Session Expired", error_message, fixed = TRUE))
  checkTrue(grepl("get_values_ranks_results(\"job123\")", error_message, fixed = TRUE))

  # an interrupt during the download says how to get the results later
  send <- fake_string_json(list(status_record("success", "Job finished", "https://example.org/download")))
  interrupted <- function(url_str, query = NULL, timeout_seconds = 300) {
    signalCondition(structure(class = c("interrupt", "condition"), list(message = "", call = NULL)))
    return(values_ranks_tsv)
  }
  shown <- character(0)
  withCallingHandlers(
    STRINGdb:::collect_values_ranks_results("u", "key1", "job123", 7200, quiet = TRUE, send = send, fetch = interrupted, sleep = fake_waits()$sleep),
    message = function(m) {
      shown <<- c(shown, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  checkTrue(any(grepl("were not downloaded", shown, fixed = TRUE) & grepl("get_values_ranks_results(\"job123\")", shown, fixed = TRUE)))

  expired <- "Error\tErrorMessage\nSession Expired\tUser-results are usually deleted after three days."
  checkEquals("Session Expired: User-results are usually deleted after three days.", STRINGdb:::string_tsv_error_message(expired))
  checkTrue(is.null(STRINGdb:::string_tsv_error_message(values_ranks_tsv)))
  quoted <- "Error\tErrorMessage\nError\tThe network &quot;bX1&quot; is <b>gone</b>."
  checkEquals("Error: The network \"bX1\" is gone .", STRINGdb:::string_tsv_error_message(quoted))
  # "&amp;" is decoded last, so a written "&amp;lt;" stays "&lt;"
  checkEquals("&lt;", STRINGdb:::decode_html_entities("&amp;lt;"))

  # get_string_text adds its query to the parameters of the address: httr's GET is traced, and the
  # tracer stops before any request is sent
  captured <- new.env()
  suppressMessages(trace("GET", where = asNamespace("STRINGdb"), print = FALSE,
    tracer = bquote({
      assign("url", url, envir = .(captured))
      assign("query", list(...)$query, envir = .(captured))
      stop("captured by the test")
    })))
  on.exit(suppressMessages(untrace("GET", where = asNamespace("STRINGdb"))), add = TRUE)
  tryCatch(STRINGdb:::get_string_text("https://example.org/download?networkId=n1", list(caller_identity = "my-tool")),
    error = function(e) NULL)
  checkEquals("https://example.org/download?networkId=n1", captured$url)
  checkEquals(list(caller_identity = "my-tool"), captured$query)

}


test_values_ranks_api_key <- function() {

  log <- new.env()
  send <- fake_string_json(list(list(list(api_key = "newkey", note = "This key will be activated within 15 minutes."))), log)
  key <- STRINGdb:::request_string_api_key("https://version-12-5.string-db.org", send = send)
  checkEquals("newkey", key$api_key)
  checkTrue(grepl("activated", key$note, fixed = TRUE))
  checkEquals("https://version-12-5.string-db.org/api/json/get_api_key", log$requests[[1]]$url)
  checkEquals(list(caller_identity = "STRINGdb-package"), log$requests[[1]]$params)

  checkException(STRINGdb:::request_string_api_key("u", send = fake_string_json(list(list()))), silent = TRUE)

  # a key that STRING is still activating
  checkTrue(grepl("up to 15 minutes", STRINGdb:::values_ranks_error_message("Authorizing the api key. Please be patient."), fixed = TRUE))

}


test_values_ranks_method_checks <- function() {

  # the methods check their input before any request to STRING
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)

  error_message <- tryCatch(string_db$get_values_ranks_enrichment("9606.A", 1, api_key = ""), error = function(e) conditionMessage(e))
  checkTrue(grepl("api_key should be one STRING API key", error_message, fixed = TRUE))

  error_message <- tryCatch(string_db$get_values_ranks_enrichment("9606.A", 1, max_fdr = 0.5, api_key = "key1"), error = function(e) conditionMessage(e))
  checkTrue(grepl("at most 0.25", error_message, fixed = TRUE))

  error_message <- tryCatch(string_db$get_values_ranks_results(c("a", NA), api_key = "key1"), error = function(e) conditionMessage(e))
  checkTrue(grepl("job_ids should be one or more job identifiers", error_message, fixed = TRUE))

  error_message <- tryCatch(string_db$get_values_ranks_status("", api_key = "key1"), error = function(e) conditionMessage(e))
  checkTrue(grepl("job_ids should be one or more job identifiers", error_message, fixed = TRUE))

  # a table of values: one row per identifier, numeric columns
  error_message <- tryCatch(
    string_db$submit_values_ranks_enrichment("9606.A", matrix(1:4, ncol = 2), api_key = "key1"),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("one row for each identifier", error_message, fixed = TRUE))

  error_message <- tryCatch(string_db$get_api_key(new = NA), error = function(e) conditionMessage(e))
  checkTrue(grepl("new should be TRUE or FALSE", error_message, fixed = TRUE))

}


test_values_ranks_method_requests <- function() {

  # get_values_ranks_enrichment, get_values_ranks_results and get_api_key send their caller_identity with every request, and
  # get_api_key asks by POST. The transport functions are replaced for this test (and restored at its
  # end), and the object points to an unreachable address, so that no request can reach STRING.
  requests <- list()
  fake_post <- function(url_str, params, timeout_seconds = 60) {
    requests[[length(requests) + 1]] <<- list(url = url_str, params = params)
    if (grepl("valuesranks_enrichment_submit", url_str, fixed = TRUE)) {
      return(list(list(job_id = "job123", status = "submitted", message = "")))
    }
    if (grepl("get_api_key", url_str, fixed = TRUE)) {
      return(list(list(api_key = "newkey", note = "")))
    }
    return(status_record("success", "Job finished", "https://example.org/download"))
  }
  fake_get_text <- function(url_str, query = NULL, timeout_seconds = 300) {
    requests[[length(requests) + 1]] <<- list(url = url_str, params = query)
    return(values_ranks_tsv)
  }
  original_post <- get("post_string_json", envir = asNamespace("STRINGdb"))
  original_get_text <- get("get_string_text", envir = asNamespace("STRINGdb"))
  assignInNamespace("post_string_json", fake_post, ns = "STRINGdb")
  assignInNamespace("get_string_text", fake_get_text, ns = "STRINGdb")
  on.exit(assignInNamespace("post_string_json", original_post, ns = "STRINGdb"), add = TRUE)
  on.exit(assignInNamespace("get_string_text", original_get_text, ns = "STRINGdb"), add = TRUE)

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"

  results <- string_db$get_values_ranks_enrichment(c("9606.A", "9606.B"), c(1.5, -2), api_key = "key1", quiet = TRUE, caller_identity = "my-tool")
  checkEquals(2, nrow(results))
  checkEquals(
    c("https://127.0.0.1:9/api/json/valuesranks_enrichment_submit", "https://127.0.0.1:9/api/json/valuesranks_enrichment_status",
      "https://example.org/download"),
    sapply(requests, function(request) request$url)
  )
  for (request in requests) {
    checkEquals("my-tool", request$params$caller_identity)
  }

  requests <- list()
  string_db$get_values_ranks_results("job123", api_key = "key1", quiet = TRUE, caller_identity = "my-tool")
  checkEquals(2, length(requests))
  for (request in requests) {
    checkEquals("my-tool", request$params$caller_identity)
  }

  # get_api_key requests a key only when this computer has none (here a temporary key file), with
  # the package's caller identity, so that the key works at once
  with_temporary_key_file({
    requests <- list()
    key <- suppressMessages(string_db$get_api_key())
    checkEquals("newkey", key)
    checkEquals(1, length(requests))
    checkEquals("https://127.0.0.1:9/api/json/get_api_key", requests[[1]]$url)
    checkEquals(list(caller_identity = "STRINGdb-package"), requests[[1]]$params)
  })

}


# A stand-in for STRING's values/ranks methods with several jobs. Submissions get the job ids
# job1, job2, ...; a status request without a job_id lists all submitted jobs, each of which has
# finished after 'rounds' status requests (successfully, or with an error for the jobs in
# 'failing'), except the jobs in 'slow', which do not finish. At most 'limit' unfinished jobs are
# taken: a submission beyond it is refused with STRING's message. 'log' keeps the requests and the
# jobs.
fake_values_ranks_server <- function(rounds = 2, failing = character(0), limit = Inf, slow = character(0),
                                     log = new.env()) {
  log$requests <- list()
  log$jobs <- list()
  function(url_str, params, timeout_seconds = 60) {
    log$requests[[length(log$requests) + 1]] <- list(url = url_str, params = params)

    if (grepl("valuesranks_enrichment_submit", url_str, fixed = TRUE)) {
      unfinished_jobs <- sum(vapply(log$jobs, function(job) !job$done, logical(1)))
      if (unfinished_jobs >= limit) {
        return(list(list(
          job_id = "", status = "error",
          message = "Job submission limit reached. Please wait for your jobs to finish before submitting new ones."
        )))
      }
      job_id <- paste("job", length(log$jobs) + 1, sep = "")
      log$jobs[[job_id]] <- list(polls = 0, done = FALSE)
      return(list(list(job_id = job_id, status = "submitted", message = "Job was successfully submitted to the queue!")))
    }

    if (grepl("valuesranks_enrichment_status", url_str, fixed = TRUE)) {
      # with a job_id, only that job; without, all jobs of the key
      listed_jobs <- names(log$jobs)
      if (!is.null(params$job_id)) {
        listed_jobs <- intersect(listed_jobs, params$job_id)
      }
      records <- list()
      for (job_id in listed_jobs) {
        job <- log$jobs[[job_id]]
        job$polls <- job$polls + 1
        job$done <- job$polls >= rounds && !(job_id %in% slow)
        log$jobs[[job_id]] <- job
        status <- "running"
        if (job$done) {
          status <- if (job_id %in% failing) "error" else "success"
        }
        records[[length(records) + 1]] <- list(
          job_id = job_id, creation_time = "2026-11-05 12:00:00", string_version = "12.5", status = status,
          message = if (status == "error") "Error. Something went wrong" else "",
          page_url = "", graph_url = "",
          download_url = if (status == "success") paste("https://example.org/download/", job_id, sep = "") else ""
        )
      }
      return(records)
    }

    if (grepl("get_api_key", url_str, fixed = TRUE)) {
      return(list(list(api_key = "newkey", note = "This key is active now.")))
    }
    stop("unexpected request ", url_str)
  }
}

# A stand-in for the download of results, which keeps the addresses asked for.
fake_download <- function(log = new.env()) {
  log$downloads <- character(0)
  function(url_str, query = NULL, timeout_seconds = 300) {
    log$downloads <- c(log$downloads, url_str)
    return(values_ranks_tsv)
  }
}

requests_to <- function(log, method) {
  return(Filter(function(request) grepl(method, request$url, fixed = TRUE), log$requests))
}

three_experiments <- function() {
  string_ids <- c("9606.A", "9606.B", "9606.C")
  values <- cbind(A = c(1.5, -2, 0.5), B = c(-1, 2, NA), C = c(3, 1, -1))
  return(STRINGdb:::prepare_values_ranks_experiments(string_ids, values))
}


test_values_ranks_api_key_sources <- function() {

  with_temporary_key_file({
    key_file <- STRINGdb:::values_ranks_key_file()
    checkTrue(startsWith(key_file, Sys.getenv("R_USER_CONFIG_DIR")))

    # without a key, the methods that submit request one, with the package's caller identity
    # (with which STRING makes it work at once), and save it
    log <- new.env()
    send <- fake_values_ranks_server(log = log)
    key_info <- suppressMessages(STRINGdb:::resolve_values_ranks_api_key(NULL, "https://s", send = send))
    checkEquals(list(key = "newkey", source = "new"), key_info)
    checkEquals("https://s/api/json/get_api_key", log$requests[[1]]$url)
    checkEquals(list(caller_identity = "STRINGdb-package"), log$requests[[1]]$params)
    checkEquals("newkey", STRINGdb:::read_saved_api_key(key_file))
    if (.Platform$OS.type == "unix") {
      checkEquals("600", format(file.info(key_file)$mode))
    }

    # then the saved key is used, without another request
    checkEquals(list(key = "newkey", source = "saved"), STRINGdb:::resolve_values_ranks_api_key(NULL, "https://s", send = send))
    checkEquals(1, length(log$requests))

    # STRINGDB_API_KEY comes before the saved key, and the argument before both
    Sys.setenv(STRINGDB_API_KEY = " envkey ")
    checkEquals(list(key = "envkey", source = "environment"), STRINGdb:::resolve_values_ranks_api_key(NULL, "https://s", send = send))
    checkEquals(list(key = "argkey", source = "argument"), STRINGdb:::resolve_values_ranks_api_key("argkey", "https://s", send = send))
    Sys.unsetenv("STRINGDB_API_KEY")

    # the methods that read jobs never request a key: a new key cannot read the jobs of another
    unlink(key_file)
    error_message <- tryCatch(
      STRINGdb:::resolve_values_ranks_api_key(NULL, "https://s", request_new = FALSE, send = send),
      error = function(e) conditionMessage(e)
    )
    checkTrue(grepl("no STRING API key on this computer", error_message, fixed = TRUE))
    checkEquals(1, length(log$requests))
  })

  # a key file that cannot be written (here: its directory is a file) gives a warning, and the key
  # is used all the same
  with_temporary_key_file({
    blocked <- file.path(Sys.getenv("R_USER_CONFIG_DIR"), "blocked")
    dir.create(dirname(blocked), recursive = TRUE)
    writeLines("not a directory", blocked)
    warned <- FALSE
    key <- withCallingHandlers(
      STRINGdb:::request_and_save_api_key("https://s", quiet = TRUE, key_file = file.path(blocked, "api_key"),
                                          send = fake_values_ranks_server()),
      warning = function(w) {
        warned <<- grepl("could not be saved", conditionMessage(w), fixed = TRUE)
        invokeRestart("muffleWarning")
      }
    )
    checkEquals("newkey", key)
    checkTrue(warned)
  })

}


test_values_ranks_saved_key_replaced <- function() {

  # a saved key that STRING no longer knows is replaced once by a new key, which is saved
  with_temporary_key_file({
    key_file <- STRINGdb:::values_ranks_key_file()
    STRINGdb:::save_api_key("oldkey", key_file)
    log <- new.env()
    log$requests <- list()
    send <- function(url_str, params, timeout_seconds = 60) {
      log$requests[[length(log$requests) + 1]] <- list(url = url_str, params = params)
      if (grepl("get_api_key", url_str, fixed = TRUE)) {
        return(list(list(api_key = "newkey", note = "")))
      }
      if (identical(params$api_key, "oldkey")) {
        return(list(list(job_id = "", status = "error", message = "Api key does not exist. Please use a valid key or generate a new one.")))
      }
      return(list(list(job_id = "job9", status = "submitted", message = "")))
    }
    key_info <- STRINGdb:::resolve_values_ranks_api_key(NULL, "https://s", send = send)
    options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
    warned <- character(0)
    submission <- suppressMessages(withCallingHandlers(
      STRINGdb:::submit_first_values_ranks_job("https://s", 9606, "9606.A\t1", key_info, options, send = send),
      warning = function(w) {
        warned <<- c(warned, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ))
    checkEquals("job9", submission$job_id)
    checkEquals("newkey", submission$key_info$key)
    checkEquals("newkey", STRINGdb:::read_saved_api_key(key_file))
    # the submission and a status request with the old key, the new key, the submission with it
    checkEquals(4, length(log$requests))
    checkTrue(grepl("valuesranks_enrichment_status", log$requests[[2]]$url, fixed = TRUE))
    # the replacement is announced with a warning that shows the old key
    checkEquals(1, length(warned))
    checkTrue(grepl("did not know the API key oldkey", warned, fixed = TRUE) && grepl("newkey", warned, fixed = TRUE))

    # a key given by the user is not replaced: the refusal stops
    error_message <- tryCatch(
      STRINGdb:::submit_first_values_ranks_job("https://s", 9606, "9606.A\t1", list(key = "oldkey", source = "argument"), options, send = send),
      error = function(e) conditionMessage(e)
    )
    checkTrue(grepl("Api key does not exist", error_message, fixed = TRUE))
    checkEquals("newkey", STRINGdb:::read_saved_api_key(key_file))
  })

  # STRING answers "does not exist" once (e.g. its key database could not be reached), but knows
  # the key at the status request: the job is submitted again with the saved key, which is kept
  with_temporary_key_file({
    key_file <- STRINGdb:::values_ranks_key_file()
    STRINGdb:::save_api_key("savedkey", key_file)
    log <- new.env()
    log$requests <- list()
    send <- function(url_str, params, timeout_seconds = 60) {
      log$requests[[length(log$requests) + 1]] <- list(url = url_str, params = params)
      if (grepl("get_api_key", url_str, fixed = TRUE)) {
        return(list(list(api_key = "newkey", note = "")))
      }
      if (grepl("status", url_str, fixed = TRUE)) {
        return(list())
      }
      if (length(log$requests) == 1) {
        return(list(list(job_id = "", status = "error", message = "Api key does not exist. Please use a valid key or generate a new one.")))
      }
      return(list(list(job_id = "job7", status = "submitted", message = "")))
    }
    key_info <- STRINGdb:::resolve_values_ranks_api_key(NULL, "https://s", send = send)
    options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
    submission <- STRINGdb:::submit_first_values_ranks_job("https://s", 9606, "9606.A\t1", key_info, options, quiet = TRUE, send = send)
    checkEquals("job7", submission$job_id)
    checkEquals("savedkey", submission$key_info$key)
    checkEquals("savedkey", STRINGdb:::read_saved_api_key(key_file))
    checkEquals(0, sum(vapply(log$requests, function(request) grepl("get_api_key", request$url, fixed = TRUE), logical(1))))
  })

}


test_values_ranks_experiments <- function() {

  # a vector is one experiment; a matrix or data frame has one experiment per column
  single <- STRINGdb:::prepare_values_ranks_experiments(c("9606.A", "9606.B"), c(1, -1))
  checkTrue(!single$several)
  checkEquals(1, length(single$experiments))

  prepared <- three_experiments()
  checkTrue(prepared$several)
  checkEquals(c("A", "B", "C"), names(prepared$experiments))
  # the input text of an experiment of a table is made when its job is submitted
  checkTrue(is.null(prepared$experiments$B$text))
  checkEquals("9606.A\t-1\n9606.B\t2", prepared$experiments$B$make_text())
  checkEquals("9606.A\t1.5\n9606.B\t-2\n9606.C\t0.5", prepared$experiments$A$make_text())
  checkEquals(1, prepared$experiments$B$n_missing)
  checkEquals("9606.A\t1\n9606.B\t-1", single$experiments[[1]]$make_text())

  unnamed <- STRINGdb:::prepare_values_ranks_experiments(c("9606.A", "9606.B"), matrix(c(1, 2, 3, 4), ncol = 2))
  checkEquals(c("experiment_1", "experiment_2"), names(unnamed$experiments))

  repeated <- STRINGdb:::prepare_values_ranks_experiments(c("9606.A", "9606.B"), data.frame(x = 1:2, x = 3:4, check.names = FALSE))
  checkEquals(c("x", "x.1"), names(repeated$experiments))

  error_message <- tryCatch(
    STRINGdb:::prepare_values_ranks_experiments(c("9606.A", "9606.B"), data.frame(up = c(1, 2), gene = c("a", "b"))),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("should be numeric", error_message, fixed = TRUE) && grepl("gene", error_message, fixed = TRUE))

  error_message <- tryCatch(
    STRINGdb:::prepare_values_ranks_experiments(c("9606.A", "9606.B"), cbind(ok = c(1, 2), empty = c(NA, NA))),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("experiment empty", error_message, fixed = TRUE))

  # messages about the rows left out: one for all experiments
  shown <- character(0)
  withCallingHandlers(
    STRINGdb:::report_left_out_rows(prepared),
    message = function(m) {
      shown <<- c(shown, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  checkEquals(1, length(shown))
  checkTrue(grepl("in 1 of 3 experiments (1 rows in all)", shown, fixed = TRUE))

}


test_values_ranks_job_ids_and_states <- function() {

  checkEquals("j1", STRINGdb:::check_values_ranks_job_ids(" j1 "))
  checkEquals(c(A = "j1", j2 = "j2"), STRINGdb:::check_values_ranks_job_ids(c(A = "j1", "j2")))
  checkException(STRINGdb:::check_values_ranks_job_ids(character(0)), silent = TRUE)
  checkException(STRINGdb:::check_values_ranks_job_ids(c("j1", NA)), silent = TRUE)
  checkException(STRINGdb:::check_values_ranks_job_ids(1), silent = TRUE)

  # one status request lists all jobs of the key
  log <- new.env()
  send <- fake_string_json(list(list(
    list(job_id = "j1", creation_time = "2026-11-05 12:00:00", status = "success", message = "", download_url = "https://d/1"),
    list(job_id = "j2", creation_time = "2026-11-05 12:01:00", status = "failure.", message = "It failed.")
  )), log)
  states <- STRINGdb:::get_values_ranks_jobs_states("https://s", "key1", "my-tool", send = send)
  checkEquals(c("j1", "j2"), names(states))
  checkEquals("failure", states$j2$status)
  checkEquals("https://d/1", states$j1$download_url)
  checkEquals(list(api_key = "key1", caller_identity = "my-tool"), log$requests[[1]]$params)

  table <- STRINGdb:::values_ranks_status_table(c(A = "j1", B = "j2", C = "j3"), states)
  checkEquals(c("experiment", "job_id", "status", "message", "creation_time"), colnames(table))
  checkEquals(c("success", "failure", "unknown"), table$status)
  checkEquals(c("A", "B", "C"), table$experiment)
  checkTrue(is.na(table$creation_time[3]))
  checkTrue(all(is.na(STRINGdb:::values_ranks_status_table("j1", states)$experiment)))

  # an empty answer: no jobs; a refusal (an error record without a job_id) stops
  checkEquals(list(), STRINGdb:::get_values_ranks_jobs_states("https://s", "key1", send = fake_string_json(list(list()))))
  refused <- fake_string_json(list(list(list(job_id = "", status = "error", message = "Api key does not exist."))))
  error <- tryCatch(STRINGdb:::get_values_ranks_jobs_states("https://s", "key1", send = refused), error = function(e) e)
  checkTrue(inherits(error, "values_ranks_refused"))
  checkTrue(grepl("keeps each job for the API key that submitted it", conditionMessage(error), fixed = TRUE))

}


test_values_ranks_run_jobs <- function() {

  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
  key_info <- list(key = "key1", source = "argument")

  # all experiments are submitted before the wait, one status request per round reports all jobs,
  # and each job's results are downloaded once it has finished
  log <- new.env()
  downloads <- new.env()
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, key_info, options, quiet = TRUE,
    send = fake_values_ranks_server(rounds = 2, log = log), fetch = fake_download(downloads),
    sleep = waits$sleep, clock = waits$clock
  )
  checkEquals(c(A = "job1", B = "job2", C = "job3"), run$job_ids)
  checkEquals(c("A", "B", "C"), sort(names(run$results)))
  checkEquals(3, length(requests_to(log, "submit")))
  checkEquals(2, length(requests_to(log, "status")))
  checkTrue(all(vapply(requests_to(log, "status"), function(request) is.null(request$params$job_id), logical(1))))
  checkEquals(paste("https://example.org/download/job", 1:3, sep = ""), sort(downloads$downloads))
  checkEquals("job2", attr(run$results$B, "job_id"))
  checkEquals(5, waits$intervals)

  # STRING takes at most 2 unfinished jobs: the third experiment is submitted once jobs have finished
  log <- new.env()
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, key_info, options, quiet = TRUE,
    send = fake_values_ranks_server(rounds = 2, limit = 2, log = log), fetch = fake_download(),
    sleep = waits$sleep, clock = waits$clock
  )
  checkEquals(c(A = "job1", B = "job2", C = "job3"), run$job_ids)
  checkEquals(3, length(run$results))
  checkEquals(0, length(run$failed))
  # the third experiment was refused (STRING's limit) before it was taken
  checkTrue(length(requests_to(log, "submit")) > 3)

  # without waiting, the run ends after the submissions, and says why the rest was held back
  log <- new.env()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, key_info, options, wait = FALSE, quiet = TRUE,
    send = fake_values_ranks_server(limit = 2, log = log), fetch = fake_download()
  )
  checkEquals(c(A = "job1", B = "job2"), run$job_ids)
  checkEquals("C", names(run$pending))
  checkTrue(grepl("Job submission limit reached", run$held_back, fixed = TRUE))
  checkEquals(0, length(requests_to(log, "status")))

  # a job that fails does not stop the others
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, key_info, options, quiet = TRUE,
    send = fake_values_ranks_server(failing = "job2"), fetch = fake_download(),
    sleep = waits$sleep, clock = waits$clock
  )
  checkEquals(c("A", "C"), sort(names(run$results)))
  checkTrue(grepl("job2 failed: Error. Something went wrong", run$failed[["B"]], fixed = TRUE))

  warned <- ""
  results <- withCallingHandlers(
    STRINGdb:::values_ranks_results_list(run, c("A", "B", "C")),
    warning = function(w) {
      warned <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  checkEquals(c("A", "B", "C"), names(results))
  checkTrue(is.null(results$B))
  checkTrue(is.data.frame(results$C))
  checkEquals(run$job_ids, attr(results, "job_ids"))
  checkTrue(grepl("1 values/ranks experiments failed: B:", warned, fixed = TRUE))

  # a refusal of the first submission (e.g. a wrong key) stops before any job is submitted
  wrong_key <- fake_string_json(list(list(list(job_id = "", status = "error", message = "Api key does not exist."))))
  error_message <- tryCatch(
    STRINGdb:::run_values_ranks_jobs("https://s", 9606, three_experiments()$experiments, key_info, options,
                                     quiet = TRUE, send = wrong_key, fetch = fake_download()),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("did not accept the values/ranks job: Api key does not exist", error_message, fixed = TRUE))

}


test_values_ranks_run_jobs_timeout <- function() {

  # the timeout ends the wait: the results so far are kept, the unfinished and the unsubmitted
  # experiments are NULL, and a warning says how to get the rest. STRING takes 2 unfinished jobs,
  # and job2 does not finish.
  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "argument"), options,
    timeout_seconds = 5, quiet = TRUE, send = fake_values_ranks_server(rounds = 2, limit = 2, slow = "job2"),
    fetch = fake_download(), sleep = waits$sleep, clock = waits$clock
  )
  checkTrue(run$timed_out)
  checkEquals(c(A = "job1", B = "job2"), run$job_ids)
  checkEquals("A", names(run$results))
  checkEquals("C", names(run$pending))

  warned <- character(0)
  results <- withCallingHandlers(
    STRINGdb:::values_ranks_results_list(run, c("A", "B", "C")),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  checkTrue(is.data.frame(results$A))
  checkTrue(is.null(results$B) && is.null(results$C))
  checkEquals(c(A = "job1", B = "job2"), attr(results, "job_ids"))
  # the column numbers of the experiments not submitted, named by experiment
  checkEquals(c(C = 3L), attr(results, "not_submitted"))
  checkEquals(2, length(warned))
  checkTrue(grepl("1 of 2 values/ranks jobs did not finish", warned[1], fixed = TRUE))
  checkTrue(grepl("within 5 seconds", warned[1], fixed = TRUE))
  checkTrue(grepl("1 experiments were not submitted", warned[2], fixed = TRUE))
  checkTrue(all(nchar(warned) < 1000))

  # nothing submitted when the timeout ends the wait: no warning about unfinished jobs
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "argument"), options,
    timeout_seconds = 5, quiet = TRUE, send = fake_values_ranks_server(limit = 0),
    fetch = fake_download(), sleep = waits$sleep, clock = waits$clock
  )
  warned <- character(0)
  results <- withCallingHandlers(
    STRINGdb:::values_ranks_results_list(run, c("A", "B", "C")),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  checkEquals(1, length(warned))
  checkTrue(grepl("3 experiments were not submitted", warned, fixed = TRUE))
  checkEquals(c(A = 1L, B = 2L, C = 3L), attr(results, "not_submitted"))

  # submissions that keep failing (HTTP 503) are given up, without losing the jobs submitted
  # before: the wait goes on for them, and the others are not submitted
  server <- fake_values_ranks_server(rounds = 1)
  log <- new.env()
  log$submissions <- 0
  failing_later <- function(url_str, params, timeout_seconds = 60) {
    if (grepl("submit", url_str, fixed = TRUE)) {
      log$submissions <- log$submissions + 1
      if (log$submissions > 2) {
        stop("ERROR: STRING request failed with HTTP status 503 for ", url_str)
      }
    }
    return(server(url_str, params, timeout_seconds))
  }
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "argument"), options,
    quiet = TRUE, send = failing_later, fetch = fake_download(), sleep = waits$sleep, clock = waits$clock
  )
  checkEquals(c("A", "B"), sort(names(run$results)))
  checkEquals("C", names(run$pending))
  checkTrue(grepl("kept failing", run$held_back, fixed = TRUE))
  checkTrue(!run$timed_out)

  # a lost connection ends the wait with an error; the job ids that the caller gave are not
  # listed again
  waits <- fake_waits()
  shown <- character(0)
  lost <- function(url_str, params, timeout_seconds = 60) stop("Timeout was reached: Operation timed out")
  error_message <- tryCatch(
    withCallingHandlers(
      STRINGdb:::run_values_ranks_jobs(
        "https://s", 9606, list(), list(key = "key1", source = "argument"), job_ids = c(A = "job1"),
        quiet = TRUE, job_ids_known = TRUE, send = lost, fetch = fake_download(),
        sleep = waits$sleep, clock = waits$clock
      ),
      message = function(m) {
        shown <<- c(shown, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("could not be read from STRING", error_message, fixed = TRUE))
  checkTrue(any(grepl("Ask again later with the same job ids", shown, fixed = TRUE)))

}


test_values_ranks_run_jobs_interrupted <- function() {

  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
  key_info <- list(key = "key1", source = "argument")

  # an interrupt during the submissions (curl stops the request with an error) lists the jobs
  # submitted so far
  server <- fake_values_ranks_server()
  aborting <- function(url_str, params, timeout_seconds = 60) {
    if (grepl("submit", url_str, fixed = TRUE) && identical(params$identifiers, "9606.A\t3\n9606.B\t1\n9606.C\t-1")) {
      stop("Operation was aborted by an application callback")
    }
    return(server(url_str, params, timeout_seconds))
  }
  shown <- character(0)
  error_message <- tryCatch(
    withCallingHandlers(
      STRINGdb:::run_values_ranks_jobs("https://s", 9606, three_experiments()$experiments, key_info, options,
                                       quiet = TRUE, send = aborting, fetch = fake_download()),
      message = function(m) {
        shown <<- c(shown, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("submission of the values/ranks jobs was interrupted", error_message, fixed = TRUE))
  checkTrue(any(grepl("get_values_ranks_results(c(A = \"job1\", B = \"job2\"))", shown, fixed = TRUE)))

  # an interrupt of R itself: its handler lists the jobs submitted until then
  server <- fake_values_ranks_server()
  interrupting <- function(url_str, params, timeout_seconds = 60) {
    if (grepl("submit", url_str, fixed = TRUE) && identical(params$identifiers, "9606.A\t3\n9606.B\t1\n9606.C\t-1")) {
      signalCondition(structure(class = c("interrupt", "condition"), list(message = "", call = NULL)))
    }
    return(server(url_str, params, timeout_seconds))
  }
  shown <- character(0)
  waits <- fake_waits()
  withCallingHandlers(
    STRINGdb:::run_values_ranks_jobs("https://s", 9606, three_experiments()$experiments, key_info, options,
                                     quiet = TRUE, send = interrupting, fetch = fake_download(),
                                     sleep = waits$sleep, clock = waits$clock),
    message = function(m) {
      shown <<- c(shown, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  checkTrue(any(grepl("keep running on STRING", shown, fixed = TRUE) &
                  grepl("c(A = \"job1\", B = \"job2\")", shown, fixed = TRUE)))

  # STRING refuses the API key for a later job: the other experiments are not tried, and the wait
  # goes on for the job submitted before
  log <- new.env()
  server <- fake_values_ranks_server(log = log)
  key_gone <- function(url_str, params, timeout_seconds = 60) {
    if (grepl("submit", url_str, fixed = TRUE) && length(requests_to(log, "submit")) >= 1) {
      log$requests[[length(log$requests) + 1]] <- list(url = url_str, params = params)
      return(list(list(job_id = "", status = "error", message = "Api key does not exist. Please use a valid key or generate a new one.")))
    }
    return(server(url_str, params, timeout_seconds))
  }
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs("https://s", 9606, three_experiments()$experiments, key_info, options,
                                          quiet = TRUE, send = key_gone, fetch = fake_download(),
                                          sleep = waits$sleep, clock = waits$clock)
  checkEquals("A", names(run$results))
  checkEquals(c("B", "C"), names(run$pending))
  checkTrue(grepl("Api key does not exist", run$held_back, fixed = TRUE))
  checkEquals(2, length(requests_to(log, "submit")))

}


test_values_ranks_run_jobs_download_retried <- function() {

  # a download that fails once is tried again in the next round
  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
  attempts <- new.env()
  attempts$job2 <- 0
  flaky_download <- function(url_str, query = NULL, timeout_seconds = 300) {
    if (grepl("job2", url_str, fixed = TRUE)) {
      attempts$job2 <- attempts$job2 + 1
      if (attempts$job2 == 1) {
        stop("ERROR: STRING request failed with HTTP status 502 for ", url_str)
      }
    }
    return(values_ranks_tsv)
  }
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "argument"), options,
    quiet = TRUE, send = fake_values_ranks_server(rounds = 1), fetch = flaky_download,
    sleep = waits$sleep, clock = waits$clock
  )
  checkEquals(c("A", "B", "C"), sort(names(run$results)))
  checkEquals(0, length(run$failed))
  checkEquals(2, attempts$job2)

}


test_values_ranks_key_replacement_safe <- function() {

  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")

  # the saved key is unknown, and STRING's queue first refuses the jobs for now: one new key is
  # requested, used in the next rounds, and saved once STRING has taken a job with it
  with_temporary_key_file({
    key_file <- STRINGdb:::values_ranks_key_file()
    STRINGdb:::save_api_key("oldkey", key_file)
    log <- new.env()
    log$requests <- list()
    log$refusals <- 0
    server <- fake_values_ranks_server()
    send <- function(url_str, params, timeout_seconds = 60) {
      if (grepl("get_api_key", url_str, fixed = TRUE)) {
        log$requests[[length(log$requests) + 1]] <- list(url = url_str, params = params)
        return(list(list(api_key = "newkey", note = "")))
      }
      if (grepl("submit", url_str, fixed = TRUE)) {
        if (identical(params$api_key, "oldkey")) {
          return(list(list(job_id = "", status = "error", message = "Api key does not exist. Please use a valid key or generate a new one.")))
        }
        if (log$refusals < 2) {
          log$refusals <- log$refusals + 1
          log$saved_while_refused <- c(log$saved_while_refused, STRINGdb:::read_saved_api_key(key_file))
          return(list(list(job_id = "", status = "error", message = "Too many jobs (> 10000) in the STRING queue")))
        }
      }
      if (grepl("status", url_str, fixed = TRUE) && identical(params$api_key, "oldkey")) {
        return(list(list(job_id = "", status = "error", message = "Api key does not exist. Please use a valid key or generate a new one.")))
      }
      return(server(url_str, params, timeout_seconds))
    }
    key_info <- STRINGdb:::resolve_values_ranks_api_key(NULL, "https://s", send = send)
    waits <- fake_waits()
    run <- suppressWarnings(suppressMessages(STRINGdb:::run_values_ranks_jobs(
      "https://s", 9606, three_experiments()$experiments, key_info, options, quiet = TRUE,
      send = send, fetch = fake_download(), sleep = waits$sleep, clock = waits$clock
    )))
    checkEquals(1, length(log$requests))
    checkEquals(3, length(run$results))
    # the saved key was kept while STRING had not taken a job with the new one
    checkEquals(c("oldkey", "oldkey"), log$saved_while_refused)
    checkEquals("newkey", STRINGdb:::read_saved_api_key(key_file))
  })

  # STRING refuses the new key as well (e.g. during an outage of its key database): the saved key
  # is kept
  with_temporary_key_file({
    key_file <- STRINGdb:::values_ranks_key_file()
    STRINGdb:::save_api_key("goodkey", key_file)
    send <- function(url_str, params, timeout_seconds = 60) {
      if (grepl("get_api_key", url_str, fixed = TRUE)) {
        return(list(list(api_key = "deadkey", note = "")))
      }
      return(list(list(job_id = "", status = "error", message = "Api key does not exist. Please use a valid key or generate a new one.")))
    }
    key_info <- STRINGdb:::resolve_values_ranks_api_key(NULL, "https://s", send = send)
    error_message <- suppressMessages(tryCatch(
      STRINGdb:::run_values_ranks_jobs("https://s", 9606, three_experiments()$experiments, key_info, options,
                                       quiet = TRUE, send = send, fetch = fake_download()),
      error = function(e) conditionMessage(e)
    ))
    checkTrue(grepl("Api key does not exist", error_message, fixed = TRUE))
    checkEquals("goodkey", STRINGdb:::read_saved_api_key(key_file))
  })

  # the first key of a computer: a key that another R session saved meanwhile is kept and used
  with_temporary_key_file({
    key_file <- STRINGdb:::values_ranks_key_file()
    checkEquals("keyA", STRINGdb:::save_api_key("keyA", key_file, replace = FALSE))
    checkEquals("keyA", STRINGdb:::save_api_key("keyB", key_file, replace = FALSE))
    checkEquals("keyB", STRINGdb:::save_api_key("keyB", key_file, replace = TRUE))
    checkEquals("api_key", list.files(dirname(key_file)))

    # a key file without a key (e.g. an empty one) is replaced
    writeLines("", key_file)
    checkEquals("keyC", STRINGdb:::save_api_key("keyC", key_file, replace = FALSE))

    unlink(key_file)
    other_session <- function(url_str, params, timeout_seconds = 60) {
      STRINGdb:::save_api_key("otherkey", key_file)
      return(list(list(api_key = "mykey", note = "")))
    }
    checkEquals("otherkey", STRINGdb:::request_and_save_api_key("https://s", quiet = TRUE, key_file = key_file, send = other_session))
  })

}


test_values_ranks_methods_several <- function() {

  # the methods with a table of values, and with several job ids; the transport functions are
  # replaced for this test (and restored at its end), the object points to an unreachable address,
  # and the key file is temporary, so that no request reaches STRING and no key is saved
  log <- new.env()
  downloads <- new.env()
  original_post <- get("post_string_json", envir = asNamespace("STRINGdb"))
  original_get_text <- get("get_string_text", envir = asNamespace("STRINGdb"))
  original_sleep <- get("VALUES_RANKS_FIRST_POLL_INTERVAL", envir = asNamespace("STRINGdb"))
  assignInNamespace("post_string_json", fake_values_ranks_server(rounds = 1, log = log), ns = "STRINGdb")
  assignInNamespace("get_string_text", fake_download(downloads), ns = "STRINGdb")
  assignInNamespace("VALUES_RANKS_FIRST_POLL_INTERVAL", 0, ns = "STRINGdb")
  on.exit(assignInNamespace("post_string_json", original_post, ns = "STRINGdb"), add = TRUE)
  on.exit(assignInNamespace("get_string_text", original_get_text, ns = "STRINGdb"), add = TRUE)
  on.exit(assignInNamespace("VALUES_RANKS_FIRST_POLL_INTERVAL", original_sleep, ns = "STRINGdb"), add = TRUE)

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"
  string_ids <- c("9606.A", "9606.B", "9606.C")
  values <- data.frame(up = c(1.5, -2, 0.5), down = c(-1, 2, 1))

  with_temporary_key_file({
    # synchronous: a named list of results; without a key, one is requested and saved first
    results <- suppressMessages(string_db$get_values_ranks_enrichment(string_ids, values, caller_identity = "my-tool"))
    checkEquals(c("up", "down"), names(results))
    checkTrue(all(vapply(results, is.data.frame, logical(1))))
    checkEquals(c(up = "job1", down = "job2"), attr(results, "job_ids"))
    key_requests <- requests_to(log, "get_api_key")
    checkEquals(1, length(key_requests))
    checkEquals("STRINGdb-package", key_requests[[1]]$params$caller_identity)
    for (request in requests_to(log, "valuesranks")) {
      checkEquals("newkey", request$params$api_key)
      checkEquals("my-tool", request$params$caller_identity)
    }
    checkEquals("newkey", STRINGdb:::read_saved_api_key(STRINGdb:::values_ranks_key_file()))

    # asynchronous: the job ids at once, their states, then their results; the saved key is used
    job_ids <- string_db$submit_values_ranks_enrichment(string_ids, values, quiet = TRUE)
    checkEquals(c(up = "job3", down = "job4"), job_ids)
    status <- string_db$get_values_ranks_status(job_ids)
    checkEquals(c("up", "down"), status$experiment)
    checkEquals(c("success", "success"), status$status)
    results <- string_db$get_values_ranks_results(job_ids, quiet = TRUE)
    checkEquals(c("up", "down"), names(results))
    checkEquals("job4", attr(results$down, "job_id"))
    checkEquals(1, length(requests_to(log, "get_api_key")))

    # one job id without a name gives a data frame, several without names a list named by job id
    checkTrue(is.data.frame(string_db$get_values_ranks_results("job3", quiet = TRUE)))
    checkEquals(c("job3", "job4"), names(string_db$get_values_ranks_results(c("job3", "job4"), quiet = TRUE)))

    # a vector of values: one job id, without a name
    job_id <- string_db$submit_values_ranks_enrichment(string_ids, values$up, quiet = TRUE)
    checkEquals("job5", job_id)

    # STRING takes only one more job: the other experiment is named in the attribute not_submitted,
    # and a short warning says why
    assignInNamespace("post_string_json", fake_values_ranks_server(limit = 1), ns = "STRINGdb")
    warned <- character(0)
    job_ids <- withCallingHandlers(
      string_db$submit_values_ranks_enrichment(string_ids, values, quiet = TRUE),
      warning = function(w) {
        warned <<- c(warned, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    checkEquals("up", names(job_ids))
    checkEquals("job1", job_ids[["up"]])
    checkEquals(c(down = 2L), attr(job_ids, "not_submitted"))
    checkTrue(grepl("1 of 2 experiments were not submitted: Job submission limit reached", warned, fixed = TRUE))
    checkTrue(nchar(warned) < 1000)
    # the job ids, with that attribute, are read as before, and the recipe gives the values left
    checkEquals(1, nrow(string_db$get_values_ranks_status(job_ids)))
    remaining <- values[, attr(job_ids, "not_submitted"), drop = FALSE]
    checkEquals("down", colnames(remaining))

    # STRING refuses the API key for a later job: the job ids so far are returned, and the
    # experiments left are not submitted
    assignInNamespace("post_string_json", function(url_str, params, timeout_seconds = 60) {
      if (identical(params$identifiers, "9606.A\t-1\n9606.B\t2\n9606.C\t1")) {
        return(list(list(job_id = "", status = "error", message = "Api key does not exist. Please use a valid key or generate a new one.")))
      }
      return(list(list(job_id = "job1", status = "submitted", message = "")))
    }, ns = "STRINGdb")
    job_ids <- suppressWarnings(string_db$submit_values_ranks_enrichment(string_ids, values, quiet = TRUE))
    checkEquals("job1", job_ids[["up"]])
    checkEquals(c(down = 2L), attr(job_ids, "not_submitted"))
  })

}


test_values_ranks_small_fixes <- function() {

  # the input text of an experiment is not kept (only its column number)
  prepared <- three_experiments()
  checkTrue(!exists("input", envir = environment(prepared$experiments$A$make_text), inherits = FALSE))
  checkEquals(1L, prepared$experiments$A$column_number)

  # a warning about many failed experiments stays under R's limit of 1000 characters
  reasons <- rep(paste(rep("STRING's values/ranks job failed: Error. Something went wrong.", 3), collapse = " "), 12)
  names(reasons) <- paste("contrast_with_a_long_name_", 1:12, sep = "")
  text <- STRINGdb:::values_ranks_experiments_warning("values/ranks experiments failed", reasons)
  checkTrue(nchar(text) < 1000)
  checkTrue(grepl("; and 9 more$", text))

  # get_api_key(new = TRUE) that cannot save the new key says so, and returns the new key
  log <- new.env()
  log$count <- 0
  keys <- function(url_str, params, timeout_seconds = 60) {
    log$count <- log$count + 1
    return(list(list(api_key = paste("key", log$count, sep = ""), note = "")))
  }
  original_post <- get("post_string_json", envir = asNamespace("STRINGdb"))
  assignInNamespace("post_string_json", keys, ns = "STRINGdb")
  on.exit(assignInNamespace("post_string_json", original_post, ns = "STRINGdb"), add = TRUE)
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"

  with_temporary_key_file({
    key_file <- STRINGdb:::values_ranks_key_file()
    suppressMessages(string_db$get_api_key())
    # the key file is replaced by a directory, so that no key can be written there
    unlink(key_file)
    dir.create(key_file)
    shown <- character(0)
    key <- withCallingHandlers(
      string_db$get_api_key(new = TRUE),
      message = function(m) {
        shown <<- c(shown, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    checkEquals("key2", key)
    checkTrue(grepl("could not be saved", shown, fixed = TRUE))
  })

}


test_values_ranks_get_api_key <- function() {

  # get_api_key requests a key only when this computer has none, keeps it, and new = TRUE replaces it
  # (showing the key it replaces)
  log <- new.env()
  log$count <- 0
  keys <- function(url_str, params, timeout_seconds = 60) {
    log$count <- log$count + 1
    return(list(list(api_key = paste("key", log$count, sep = ""), note = "This key is active now.")))
  }
  original_post <- get("post_string_json", envir = asNamespace("STRINGdb"))
  assignInNamespace("post_string_json", keys, ns = "STRINGdb")
  on.exit(assignInNamespace("post_string_json", original_post, ns = "STRINGdb"), add = TRUE)

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"

  with_temporary_key_file({
    shown <- character(0)
    keep_messages <- function(expression) {
      withCallingHandlers(expression, message = function(m) {
        shown <<- c(shown, conditionMessage(m))
        invokeRestart("muffleMessage")
      })
    }
    checkEquals("key1", keep_messages(string_db$get_api_key()))
    checkEquals("key1", keep_messages(string_db$get_api_key()))
    checkEquals(1, log$count)
    checkEquals("key2", keep_messages(string_db$get_api_key(new = TRUE)))
    checkTrue(grepl("It replaces the key key1", shown[3], fixed = TRUE))
    checkEquals("key2", STRINGdb:::read_saved_api_key(STRINGdb:::values_ranks_key_file()))

    # STRINGDB_API_KEY comes first, without a request
    Sys.setenv(STRINGDB_API_KEY = "envkey")
    checkEquals("envkey", keep_messages(string_db$get_api_key()))
    checkEquals(2, log$count)
  })

}


test_values_ranks_single_waits_and_long_hint <- function() {

  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
  key_info <- list(key = "key1", source = "argument")

  # one experiment that STRING cannot take now (its limit of active jobs) is submitted as soon as
  # STRING takes it, within the timeout
  log <- new.env()
  log$submissions <- 0
  limit_twice <- function(url_str, params, timeout_seconds = 60) {
    log$submissions <- log$submissions + 1
    if (log$submissions <= 2) {
      return(list(list(job_id = "", status = "error", message = "Job submission limit reached. Please wait for your jobs to finish before submitting new ones.")))
    }
    return(list(list(job_id = "job5", status = "submitted", message = "")))
  }
  waits <- fake_waits()
  shown <- character(0)
  submission <- withCallingHandlers(
    STRINGdb:::submit_values_ranks_job_when_possible("https://s", 9606, "9606.A\t1", key_info, options, 600,
                                                     send = limit_twice, sleep = waits$sleep, clock = waits$clock),
    message = function(m) {
      shown <<- c(shown, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  checkEquals("job5", submission$job_id)
  checkEquals(3, log$submissions)
  checkEquals(12.5, submission$waited)
  checkEquals(1, length(shown))
  checkTrue(grepl("submitted as soon as STRING takes it", shown, fixed = TRUE))

  # the timeout ends the wait for a submission with an error
  waits <- fake_waits()
  always_full <- function(url_str, params, timeout_seconds = 60) {
    return(list(list(job_id = "", status = "error", message = "Too many jobs (> 10000) in the STRING queue")))
  }
  error_message <- tryCatch(
    STRINGdb:::submit_values_ranks_job_when_possible("https://s", 9606, "9606.A\t1", key_info, options, 10, quiet = TRUE,
                                                     send = always_full, sleep = waits$sleep, clock = waits$clock),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("did not take the values/ranks job within 10 seconds: Too many jobs", error_message, fixed = TRUE))

  # other refusals stop at once
  wrong_species <- function(url_str, params, timeout_seconds = 60) {
    return(list(list(job_id = "", status = "error", message = "Species not found.")))
  }
  waits <- fake_waits()
  error_message <- tryCatch(
    STRINGdb:::submit_values_ranks_job_when_possible("https://s", 9606, "9606.A\t1", key_info, options, 600, quiet = TRUE,
                                                     send = wrong_species, sleep = waits$sleep, clock = waits$clock),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("Species not found", error_message, fixed = TRUE))
  checkEquals(numeric(0), waits$intervals)

  # many job ids: saved in a file of the R session instead of a long message
  job_ids <- paste("job", 1:25, sep = "")
  names(job_ids) <- paste("experiment", 1:25, sep = "_")
  hint <- STRINGdb:::values_ranks_jobs_hint(job_ids)
  checkTrue(nchar(hint) < 400)
  job_ids_file <- regmatches(hint, regexpr("readRDS\\(\"[^\"]+\"\\)", hint))
  job_ids_file <- sub("^readRDS\\(\"", "", sub("\"\\)$", "", job_ids_file))
  checkTrue(file.exists(job_ids_file))
  checkEquals(job_ids, readRDS(job_ids_file))
  unlink(job_ids_file)

  # a few job ids are listed in the message itself
  checkTrue(grepl("c(A = \"job1\", B = \"job2\")", STRINGdb:::values_ranks_jobs_hint(c(A = "job1", B = "job2")), fixed = TRUE))

}
