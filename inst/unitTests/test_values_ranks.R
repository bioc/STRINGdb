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

  error_message <- tryCatch(STRINGdb:::resolve_values_ranks_api_key(""), error = function(e) conditionMessage(e))
  checkTrue(grepl("get_api_key", error_message, fixed = TRUE))
  checkEquals("abc", STRINGdb:::resolve_values_ranks_api_key(" abc "))

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
  checkTrue(grepl("needs an API key", error_message, fixed = TRUE))

  error_message <- tryCatch(string_db$get_values_ranks_enrichment("9606.A", 1, max_fdr = 0.5, api_key = "key1"), error = function(e) conditionMessage(e))
  checkTrue(grepl("at most 0.25", error_message, fixed = TRUE))

  error_message <- tryCatch(string_db$get_values_ranks_results(c("a", "b"), api_key = "key1"), error = function(e) conditionMessage(e))
  checkTrue(grepl("one job identifier", error_message, fixed = TRUE))

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

  requests <- list()
  key <- suppressMessages(string_db$get_api_key(caller_identity = "my-tool"))
  checkEquals("newkey", key)
  checkEquals(1, length(requests))
  checkEquals("https://127.0.0.1:9/api/json/get_api_key", requests[[1]]$url)
  checkEquals(list(caller_identity = "my-tool"), requests[[1]]$params)

}
