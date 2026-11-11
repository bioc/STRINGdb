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
  # NA identifier, empty identifier and NA value are left out, and the infinite value is counted
  # apart (it gets a warning: test_values_ranks_input_checks); 9606.A keeps -2.5
  checkEquals(3, input$n_missing)
  checkEquals(1, input$n_infinite)
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
  checkTrue(grepl("servers may be busy", error_message, fixed = TRUE))
  checkTrue(grepl("please contact STRING support", error_message, fixed = TRUE))

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
  checkEquals(list(api_key = "key1", job_id = "job123"), log$requests[[1]]$params)

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

  # the results have the columns of get_enrichment where they mean the same
  results <- STRINGdb:::read_values_ranks_results(values_ranks_tsv, "job123")
  checkEquals(
    c("category", "term", "number_of_genes", "number_of_genes_in_background", "inputGenes", "preferredNames",
      "enrichment_score", "direction", "method", "fdr", "description", "inputValues", "inputRanks"),
    colnames(results)
  )
  checkEquals(STRINGdb:::VALUES_RANKS_RESULT_COLUMNS, colnames(results))
  checkEquals(2, nrow(results))
  checkEquals(c("Process", "KEGG"), results$category)
  checkEquals(c("GO:0006281", "hsa04110"), results$term)
  checkEquals("DNA repair # and more", results$description[1])
  checkEquals(c(3L, 2L), results$number_of_genes)
  checkEquals(c(517L, 157L), results$number_of_genes_in_background)
  checkEquals(c(1.25, -0.8), results$enrichment_score)
  checkEquals(c(0.0012, 0.03), results$fdr)
  checkEquals(c("top", "bottom"), results$direction)
  checkEquals(c("ks", "afc"), results$method)

  # inputGenes and preferredNames are comma-separated text, as in get_enrichment; the values and
  # ranks are list columns of numbers, in the order of inputGenes
  checkEquals("9606.A,9606.B,9606.C", results$inputGenes[1])
  checkEquals("A,B,C", results$preferredNames[1])
  checkTrue(is.list(results$inputValues) && is.list(results$inputRanks))
  checkEquals(list(c(2.1, 1.9, 1.5), c(-1.2, -2.2)), results$inputValues)
  checkEquals(list(1:3, 5:6), results$inputRanks)
  checkTrue(is.integer(results$inputRanks[[1]]))

  # the text columns stay text, also a label that reads as a number or as NA
  numeric_looking <- paste(
    strsplit(values_ranks_tsv, "\n")[[1]][1],
    "KEGG\thsa04110\tNA\t2\t157\t-0.8\tbottom\t0.03\tafc\t9606.D,9606.E\t1\tNA\t-1.2,-2.2\t5,6",
    sep = "\n"
  )
  results <- STRINGdb:::read_values_ranks_results(numeric_looking, "job123")
  checkEquals("NA", results$description)
  checkEquals("1", results$preferredNames)

  # a table without terms has typed columns and no rows
  empty <- STRINGdb:::read_values_ranks_results(strsplit(values_ranks_tsv, "\n")[[1]][1], "job123")
  checkEquals(STRINGdb:::VALUES_RANKS_RESULT_COLUMNS, colnames(empty))
  checkEquals(0, nrow(empty))
  checkTrue(is.integer(empty$number_of_genes))
  checkTrue(is.numeric(empty$fdr))
  checkTrue(is.list(empty$inputValues))

  # STRING's display names of the categories become the codes of get_enrichment; "Monarch" stands for
  # the phenotype ontologies, told apart by the prefix of the term; unknown names stay
  checkEquals(
    c("Process", "Function", "Component", "Keyword", "RCTM", "PMID", "NetworkNeighborAL", "COMPARTMENTS"),
    STRINGdb:::values_ranks_category_codes(
      c("GO Process", "GO Function", "GO Component", "UniProt Keywords", "Reactome", "Publications",
        "STRING clusters", "COMPARTMENTS"),
      rep("x", 8)
    )
  )
  checkEquals(
    c("HPO", "MPO", "DPO", "WPO", "WPO", "ZPO", "FYPO", "Monarch", "New category"),
    STRINGdb:::values_ranks_category_codes(
      c(rep("Monarch", 8), "New category"),
      c("HP:0000118", "MP:0005376", "FBcv:0000347", "WBPhenotype:0000886", "WBP:0000886", "ZP:0000001",
        "FYPO:0000001", "XX:1", "T1")
    )
  )

  # a table without one of STRING's columns is not read as results
  without_ranks <- sub("\tproteinRanks", "", strsplit(values_ranks_tsv, "\n")[[1]][1])
  error_message <- tryCatch(
    STRINGdb:::read_values_ranks_results(without_ranks, "job123"),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("lack the columns proteinRanks", error_message, fixed = TRUE))

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

  # a download that fails is tried again, a few times
  attempts <- 0
  flaky <- function(url_str, query = NULL, timeout_seconds = 300) {
    attempts <<- attempts + 1
    if (attempts == 1) {
      stop("ERROR: STRING's servers answered with HTTP status 502 for ", url_str, call. = FALSE)
    }
    return(values_ranks_tsv)
  }
  results <- STRINGdb:::collect_values_ranks_results("u", "key1", "job123", 7200, quiet = TRUE, send = send, fetch = flaky,
                                                     sleep = fake_waits()$sleep)
  checkEquals(2, attempts)
  checkEquals(2, nrow(results))
  attempts <- 0
  always_failing <- function(url_str, query = NULL, timeout_seconds = 300) {
    attempts <<- attempts + 1
    stop("ERROR: STRING's servers answered with HTTP status 502 for ", url_str, call. = FALSE)
  }
  tryCatch(STRINGdb:::collect_values_ranks_results("u", "key1", "job123", 7200, quiet = TRUE, send = send,
                                                   fetch = always_failing, sleep = fake_waits()$sleep),
    error = function(e) NULL)
  checkEquals(5, attempts)

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

}


test_values_ranks_api_key <- function() {

  log <- new.env()
  send <- fake_string_json(list(list(list(api_key = "newkey", note = "This key will be activated within 15 minutes."))), log)
  key <- STRINGdb:::request_string_api_key("https://version-12-5.string-db.org", send = send)
  checkEquals("newkey", key$api_key)
  checkTrue(grepl("activated", key$note, fixed = TRUE))
  checkEquals("https://version-12-5.string-db.org/api/json/get_api_key", log$requests[[1]]$url)
  checkEquals(list(), log$requests[[1]]$params)

  checkException(STRINGdb:::request_string_api_key("u", send = fake_string_json(list(list()))), silent = TRUE)

  # a key that STRING is still activating
  checkTrue(grepl("up to 15 minutes", STRINGdb:::values_ranks_error_message("Authorizing the api key. Please be patient."), fixed = TRUE))

}


test_values_ranks_method_checks <- function() {

  # the methods check their input before any request to STRING. The object points to an unreachable
  # address, and the key is a temporary one (STRINGDB_API_KEY), so that a check that does not stop
  # reaches neither STRING nor the key of this computer.
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"

  with_temporary_key_file({
    Sys.setenv(STRINGDB_API_KEY = "key1")

    error_message <- tryCatch(string_db$get_values_ranks_enrichment("9606.A", 1, max_fdr = 0.5), error = function(e) conditionMessage(e))
    checkTrue(grepl("at most 0.25", error_message, fixed = TRUE))

    error_message <- tryCatch(string_db$get_values_ranks_results(c("a", NA)), error = function(e) conditionMessage(e))
    checkTrue(grepl("job_ids should be one or more job identifiers", error_message, fixed = TRUE))

    error_message <- tryCatch(string_db$get_values_ranks_status(""), error = function(e) conditionMessage(e))
    checkTrue(grepl("job_ids should be one or more job identifiers", error_message, fixed = TRUE))

    # a table of values: one row per identifier, numeric columns
    error_message <- tryCatch(
      string_db$submit_values_ranks_enrichment("9606.A", matrix(1:4, ncol = 2)),
      error = function(e) conditionMessage(e)
    )
    checkTrue(grepl("one row for each identifier", error_message, fixed = TRUE))
  })

}


test_values_ranks_method_requests <- function() {

  # the requests of get_values_ranks_enrichment, get_values_ranks_results and get_api_key (which
  # asks by POST). The transport functions are replaced for this test (and restored at its end),
  # and the object points to an unreachable address, so that no request can reach STRING.
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

  # the key of STRINGDB_API_KEY (a temporary setting) is the key of the requests
  with_temporary_key_file({
    Sys.setenv(STRINGDB_API_KEY = "key1")
    results <- string_db$get_values_ranks_enrichment(c("9606.A", "9606.B"), c(1.5, -2), quiet = TRUE)
    checkEquals(2, nrow(results))
    checkEquals(
      c("https://127.0.0.1:9/api/json/valuesranks_enrichment_submit", "https://127.0.0.1:9/api/json/valuesranks_enrichment_status",
        "https://example.org/download"),
      sapply(requests, function(request) request$url)
    )
    checkEquals("key1", requests[[1]]$params$api_key)
    # the default FDR cutoff
    checkEquals("0.05", requests[[1]]$params$ge_fdr)

    requests <- list()
    string_db$get_values_ranks_results("job123", quiet = TRUE)
    checkEquals(2, length(requests))
    checkEquals("key1", requests[[1]]$params$api_key)
  })


  # get_api_key requests a key only when this computer has none (here a temporary key file)
  with_temporary_key_file({
    requests <- list()
    key <- suppressMessages(string_db$get_api_key())
    checkEquals("newkey", key)
    checkEquals(1, length(requests))
    checkEquals("https://127.0.0.1:9/api/json/get_api_key", requests[[1]]$url)
    checkEquals(list(), requests[[1]]$params)
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

    # without a key, the methods that submit request one and save it
    log <- new.env()
    send <- fake_values_ranks_server(log = log)
    key_info <- suppressMessages(STRINGdb:::resolve_values_ranks_api_key("https://s", send = send))
    checkEquals(list(key = "newkey", source = "new"), key_info)
    checkEquals("https://s/api/json/get_api_key", log$requests[[1]]$url)
    checkEquals(list(), log$requests[[1]]$params)
    checkEquals("newkey", STRINGdb:::read_saved_api_key(key_file))
    if (.Platform$OS.type == "unix") {
      checkEquals("600", format(file.info(key_file)$mode))
    }

    # then the saved key is used, without another request
    checkEquals(list(key = "newkey", source = "saved"), STRINGdb:::resolve_values_ranks_api_key("https://s", send = send))
    checkEquals(1, length(log$requests))

    # STRINGDB_API_KEY comes before the saved key
    Sys.setenv(STRINGDB_API_KEY = " envkey ")
    checkEquals(list(key = "envkey", source = "environment"), STRINGdb:::resolve_values_ranks_api_key("https://s", send = send))
    Sys.unsetenv("STRINGDB_API_KEY")

    # the methods that read jobs never request a key: a new key cannot read the jobs of another
    unlink(key_file)
    error_message <- tryCatch(
      STRINGdb:::resolve_values_ranks_api_key("https://s", request_new = FALSE, send = send),
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
    key_info <- STRINGdb:::resolve_values_ranks_api_key("https://s", send = send)
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
      STRINGdb:::submit_first_values_ranks_job("https://s", 9606, "9606.A\t1", list(key = "oldkey", source = "environment"), options, send = send),
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
    key_info <- STRINGdb:::resolve_values_ranks_api_key("https://s", send = send)
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
  states <- STRINGdb:::get_values_ranks_jobs_states("https://s", "key1", send = send)
  checkEquals(c("j1", "j2"), names(states))
  checkEquals("failure", states$j2$status)
  checkEquals("https://d/1", states$j1$download_url)
  checkEquals(list(api_key = "key1"), log$requests[[1]]$params)

  table <- STRINGdb:::values_ranks_status_table(c(A = "j1", B = "j2", C = "j3"), states)
  checkEquals(c("experiment", "job_id", "status", "message", "creation_time"), colnames(table))
  checkEquals(c("success", "failed", "unknown"), table$status)

  # STRING's own states are reported as one of a fixed set
  raw_states <- c("submitted", "preparing", "running", "success", "done", "error", "failure.", "failed", "expired", "new state")
  checkEquals(
    c("submitted", "submitted", "running", "success", "success", "failed", "failed", "failed", "failed", "running"),
    vapply(raw_states, STRINGdb:::values_ranks_status_of, character(1), USE.NAMES = FALSE)
  )

  # without job ids, all jobs that STRING lists for the key, oldest first; no jobs give no rows
  newest_first <- list(
    j2 = list(status = "running", message = "", creation_time = "2026-11-05 12:01:00"),
    j1 = list(status = "success", message = "", creation_time = "2026-11-05 12:00:00")
  )
  all_jobs <- STRINGdb:::values_ranks_status_table(NULL, newest_first)
  checkEquals(c("j1", "j2"), all_jobs$job_id)
  checkEquals(c("success", "running"), all_jobs$status)
  checkTrue(all(is.na(all_jobs$experiment)))
  no_jobs <- STRINGdb:::values_ranks_status_table(NULL, list())
  checkEquals(0, nrow(no_jobs))
  checkEquals(c("experiment", "job_id", "status", "message", "creation_time"), colnames(no_jobs))
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
  key_info <- list(key = "key1", source = "environment")

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
  checkTrue(grepl("1 values/ranks experiment failed: B:", warned, fixed = TRUE))
  checkEquals(c(A = "success", B = "failed", C = "success"), attr(results, "status"))

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
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "environment"), options,
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
  checkEquals(c(A = "success", B = "unfinished", C = "not submitted"), attr(results, "status"))
  checkEquals(2, length(warned))
  checkTrue(grepl("1 of 2 values/ranks jobs did not finish", warned[1], fixed = TRUE))
  checkTrue(grepl("within 5 seconds each", warned[1], fixed = TRUE))
  checkTrue(grepl("please contact STRING support", warned[1], fixed = TRUE))
  checkTrue(grepl("Not submitted: 1 of 3 experiments.", warned[2], fixed = TRUE))
  checkTrue(grepl("because this API key already has its maximum of 1000 jobs", warned[2], fixed = TRUE))
  checkTrue(!grepl("contact STRING support", warned[2], fixed = TRUE))
  checkTrue(all(nchar(warned) < 1000))

  # nothing submitted when the timeout ends the wait: no warning about unfinished jobs
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "environment"), options,
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
  checkTrue(grepl("Not submitted: 3 of 3 experiments.", warned, fixed = TRUE))
  checkTrue(grepl("did not take them within 5 seconds, because this API key already has its maximum", warned, fixed = TRUE))
  checkTrue(grepl("Submit again once some of your jobs have finished.", warned, fixed = TRUE))
  checkTrue(!grepl("contact STRING support", warned, fixed = TRUE))

  # a full STRING queue: STRING may be busy, so the warning says to try later and gives the support
  queue_full <- function(url_str, params, timeout_seconds = 60) {
    if (grepl("submit", url_str, fixed = TRUE)) {
      return(list(list(job_id = "", status = "error", message = "Too many jobs (> 10000) in the STRING queue")))
    }
    return(list())
  }
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "environment"), options,
    timeout_seconds = 5, quiet = TRUE, send = queue_full, fetch = fake_download(), sleep = waits$sleep, clock = waits$clock
  )
  warned <- character(0)
  withCallingHandlers(
    STRINGdb:::values_ranks_results_list(run, c("A", "B", "C")),
    warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  checkEquals(1, length(warned))
  checkTrue(grepl("STRING's answer: Too many jobs (> 10000) in the STRING queue.", warned, fixed = TRUE))
  checkTrue(grepl("please try again later", warned, fixed = TRUE))
  checkTrue(grepl("please contact STRING support", warned, fixed = TRUE))
  checkEquals(c(A = 1L, B = 2L, C = 3L), attr(results, "not_submitted"))
  checkEquals(c(A = "not submitted", B = "not submitted", C = "not submitted"), attr(results, "status"))

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
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "environment"), options,
    quiet = TRUE, send = failing_later, fetch = fake_download(), sleep = waits$sleep, clock = waits$clock
  )
  checkEquals(c("A", "B"), sort(names(run$results)))
  checkEquals("C", names(run$pending))
  checkTrue(grepl("kept failing", run$held_back, fixed = TRUE))
  checkTrue(!run$timed_out)

  # without waiting, a submission request that fails is tried again, as with waiting, and one that
  # keeps failing is given up
  failing_once <- function(url_str, params, timeout_seconds = 60) {
    if (grepl("submit", url_str, fixed = TRUE)) {
      log$submissions <- log$submissions + 1
      if (log$submissions == 2) {
        stop("Timeout was reached: Operation timed out")
      }
    }
    return(server(url_str, params, timeout_seconds))
  }
  log$submissions <- 0
  server <- fake_values_ranks_server(rounds = 1)
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "environment"), options,
    wait = FALSE, quiet = TRUE, send = failing_once, fetch = fake_download(), sleep = fake_waits()$sleep
  )
  checkEquals(c("A", "B", "C"), names(run$job_ids))
  checkEquals(0, length(run$pending))
  checkEquals(4, log$submissions)

  log$submissions <- 0
  server <- fake_values_ranks_server(rounds = 1)
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "environment"), options,
    wait = FALSE, quiet = TRUE, send = failing_later, fetch = fake_download(), sleep = fake_waits()$sleep
  )
  checkEquals(c("A", "B"), names(run$job_ids))
  checkEquals("C", names(run$pending))
  checkTrue(grepl("kept failing", run$held_back, fixed = TRUE))

  # a lost connection ends the wait with an error; the job ids that the caller gave are not
  # listed again
  waits <- fake_waits()
  shown <- character(0)
  lost <- function(url_str, params, timeout_seconds = 60) stop("Timeout was reached: Operation timed out")
  error_message <- tryCatch(
    withCallingHandlers(
      STRINGdb:::run_values_ranks_jobs(
        "https://s", 9606, list(), list(key = "key1", source = "environment"), job_ids = c(A = "job1"),
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
  key_info <- list(key = "key1", source = "environment")

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
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "environment"), options,
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
    key_info <- STRINGdb:::resolve_values_ranks_api_key("https://s", send = send)
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
    key_info <- STRINGdb:::resolve_values_ranks_api_key("https://s", send = send)
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
    results <- suppressMessages(string_db$get_values_ranks_enrichment(string_ids, values))
    checkEquals(c("up", "down"), names(results))
    checkTrue(all(vapply(results, is.data.frame, logical(1))))
    checkEquals(c(up = "job1", down = "job2"), attr(results, "job_ids"))
    checkEquals(c(up = "success", down = "success"), attr(results, "status"))
    checkEquals(1, length(requests_to(log, "get_api_key")))
    for (request in requests_to(log, "valuesranks")) {
      checkEquals("newkey", request$params$api_key)
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
    checkEquals(c(up = "success", down = "success"), attr(results, "status"))
    checkEquals(1, length(requests_to(log, "get_api_key")))

    # one job id without a name gives a data frame, several without names a list named by job id
    checkTrue(is.data.frame(string_db$get_values_ranks_results("job3", quiet = TRUE)))
    checkEquals(c("job3", "job4"), names(string_db$get_values_ranks_results(c("job3", "job4"), quiet = TRUE)))

    # a vector of values: one job id, without a name
    job_id <- string_db$submit_values_ranks_enrichment(string_ids, values$up, quiet = TRUE)
    checkEquals("job5", job_id)

    # a table of one column has job ids named by experiment, whose results are a list, as
    # get_values_ranks_enrichment gives for such a table
    one_column_ids <- string_db$submit_values_ranks_enrichment(string_ids, values[, "up", drop = FALSE], quiet = TRUE)
    checkEquals("up", names(one_column_ids))
    one_column_results <- string_db$get_values_ranks_results(one_column_ids, quiet = TRUE)
    checkTrue(is.list(one_column_results) && !is.data.frame(one_column_results))
    checkEquals("up", names(one_column_results))

    # without job ids, the states of all jobs of the key
    all_jobs <- string_db$get_values_ranks_status()
    checkTrue(all(c("job1", "job2", "job3", "job4", "job5", "job6") %in% all_jobs$job_id))
    checkTrue(all(is.na(all_jobs$experiment)))

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
    checkTrue(grepl("Not submitted: 1 of 2 experiments. STRING did not take them now, because this API key already has", warned, fixed = TRUE))
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

  # get_api_key that cannot save the new key says so, and returns the new key
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
    # a directory in place of the key file, so that no key can be written there
    dir.create(key_file, recursive = TRUE)
    shown <- character(0)
    key <- withCallingHandlers(
      string_db$get_api_key(),
      message = function(m) {
        shown <<- c(shown, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    checkEquals("key1", key)
    checkTrue(grepl("could not be saved", shown, fixed = TRUE))
  })

}


test_values_ranks_get_api_key <- function() {

  # get_api_key requests a key only when this computer has none, and keeps it
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
    checkEquals("key1", STRINGdb:::read_saved_api_key(STRINGdb:::values_ranks_key_file()))

    # STRINGDB_API_KEY comes first, without a request
    Sys.setenv(STRINGDB_API_KEY = "envkey")
    checkEquals("envkey", keep_messages(string_db$get_api_key()))
    checkEquals(1, log$count)
  })

}


test_values_ranks_single_waits_and_long_hint <- function() {

  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
  key_info <- list(key = "key1", source = "environment")

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
  checkTrue(grepl("did not take the values/ranks job within 10 seconds. STRING's answer: Too many jobs", error_message, fixed = TRUE))
  checkTrue(grepl("please try again later", error_message, fixed = TRUE))
  checkTrue(grepl("please contact STRING support", error_message, fixed = TRUE))

  # the key's own limit of jobs: submit again once jobs have finished, without the support hint
  waits <- fake_waits()
  key_full <- function(url_str, params, timeout_seconds = 60) {
    return(list(list(job_id = "", status = "error",
                     message = "Job submission limit reached. Please wait for your jobs to finish before submitting new ones.")))
  }
  error_message <- tryCatch(
    STRINGdb:::submit_values_ranks_job_when_possible("https://s", 9606, "9606.A\t1", key_info, options, 10, quiet = TRUE,
                                                     send = key_full, sleep = waits$sleep, clock = waits$clock),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("within 10 seconds, because this API key already has its maximum of 1000 jobs", error_message, fixed = TRUE))
  checkTrue(grepl("Submit again once some of your jobs have finished.", error_message, fixed = TRUE))
  checkTrue(!grepl("contact STRING support", error_message, fixed = TRUE))

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



test_values_ranks_input_alone <- function() {

  # a numeric vector named by STRING identifiers
  input <- STRINGdb:::values_ranks_input_of(c("9606.A" = 1.5, "9606.B" = -2))
  checkEquals(c("9606.A", "9606.B"), input$string_ids)
  checkEquals(c(1.5, -2), input$values)

  # also given as values, without string_ids
  checkEquals(input, STRINGdb:::values_ranks_input_of(NULL, c("9606.A" = 1.5, "9606.B" = -2)))

  # a matrix or a data frame with STRING identifiers as row names
  table <- matrix(c(1, 2, 3, 4), ncol = 2, dimnames = list(c("9606.A", "9606.B"), c("up", "down")))
  input <- STRINGdb:::values_ranks_input_of(table)
  checkEquals(c("9606.A", "9606.B"), input$string_ids)
  checkEquals(table, input$values)
  input <- STRINGdb:::values_ranks_input_of(as.data.frame(table))
  checkEquals(c("9606.A", "9606.B"), input$string_ids)

  # both given: as before
  input <- STRINGdb:::values_ranks_input_of(c("9606.A", "9606.B"), c(1, 2))
  checkEquals(c(1, 2), input$values)

  # without names, the identifiers are unknown
  error_message <- tryCatch(STRINGdb:::values_ranks_input_of(c(1.5, -2)), error = function(e) conditionMessage(e))
  checkTrue(grepl("have no names", error_message, fixed = TRUE) && grepl("setNames", error_message, fixed = TRUE))
  error_message <- tryCatch(STRINGdb:::values_ranks_input_of(data.frame(up = 1:2)), error = function(e) conditionMessage(e))
  checkTrue(grepl("no row names", error_message, fixed = TRUE))
  error_message <- tryCatch(STRINGdb:::values_ranks_input_of(c("9606.A", "9606.B")), error = function(e) conditionMessage(e))
  checkTrue(grepl("should be numeric", error_message, fixed = TRUE))
  error_message <- tryCatch(STRINGdb:::values_ranks_input_of(), error = function(e) conditionMessage(e))
  checkTrue(grepl("no values to rank", error_message, fixed = TRUE))

  # the methods take the values alone: one job for a named vector, one per column for a table
  log <- new.env()
  original_post <- get("post_string_json", envir = asNamespace("STRINGdb"))
  assignInNamespace("post_string_json", fake_values_ranks_server(log = log), ns = "STRINGdb")
  on.exit(assignInNamespace("post_string_json", original_post, ns = "STRINGdb"), add = TRUE)
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"

  with_temporary_key_file({
    Sys.setenv(STRINGDB_API_KEY = "key1")
    job_id <- string_db$submit_values_ranks_enrichment(c("9606.A" = 1.5, "9606.B" = -2), quiet = TRUE)
    checkEquals("job1", job_id)
    checkEquals("9606.A\t1.5\n9606.B\t-2", requests_to(log, "submit")[[1]]$params$identifiers)

    job_ids <- string_db$submit_values_ranks_enrichment(values = table, quiet = TRUE)
    checkEquals(c(up = "job2", down = "job3"), job_ids)
    checkEquals("9606.A\t3\n9606.B\t4", requests_to(log, "submit")[[3]]$params$identifiers)
  })

}


test_values_ranks_input_alone_more <- function() {

  # row names that are the numbers of the rows are not STRING identifiers: those that a data frame
  # keeps after subsetting, its automatic ones, and a matrix's digits
  subsetted <- subset(data.frame(up = 1:4, down = 4:1), up != 2)
  for (table in list(subsetted, data.frame(up = 1:2), as.matrix(subsetted), matrix(1:4, 2))) {
    error_message <- tryCatch(STRINGdb:::values_ranks_input_of(table), error = function(e) conditionMessage(e))
    checkTrue(grepl("no row names that could be STRING identifiers", error_message, fixed = TRUE))
  }
  real_names <- data.frame(up = 1:2, row.names = c("9606.A", "9606.B"))
  checkEquals(c("9606.A", "9606.B"), STRINGdb:::values_ranks_input_of(real_names)$string_ids)

  # a named 1-d array, as tapply gives it (also sorted), is a named vector
  ranked <- sort(tapply(c(1, -2, 3, 7), c("9606.B", "9606.A", "9606.C", "9606.B"), mean), decreasing = TRUE)
  input <- STRINGdb:::values_ranks_input_of(ranked)
  checkEquals(c("9606.B", "9606.C", "9606.A"), input$string_ids)
  checkEquals(c(4, 3, -2), input$values)
  checkTrue(is.null(dim(input$values)))

  # values given alone, followed by an option by position, are not taken for identifiers and values
  error_message <- tryCatch(
    STRINGdb:::values_ranks_input_of(c("9606.A" = 1, "9606.B" = 2), 0.05),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("give the other options by name", error_message, fixed = TRUE))

  # get_values_ranks_enrichment with the values alone: the job of the named vector, and its results
  log <- new.env()
  original_post <- get("post_string_json", envir = asNamespace("STRINGdb"))
  original_get_text <- get("get_string_text", envir = asNamespace("STRINGdb"))
  assignInNamespace("post_string_json", function(url_str, params, timeout_seconds = 60) {
    log$requests[[length(log$requests) + 1]] <- list(url = url_str, params = params)
    if (grepl("submit", url_str, fixed = TRUE)) {
      return(list(list(job_id = "job123", status = "submitted", message = "")))
    }
    return(status_record("success", "", "https://example.org/download"))
  }, ns = "STRINGdb")
  assignInNamespace("get_string_text", function(url_str, query = NULL, timeout_seconds = 300) values_ranks_tsv, ns = "STRINGdb")
  on.exit(assignInNamespace("post_string_json", original_post, ns = "STRINGdb"), add = TRUE)
  on.exit(assignInNamespace("get_string_text", original_get_text, ns = "STRINGdb"), add = TRUE)
  log$requests <- list()

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"
  results <- with_temporary_key_file({
    Sys.setenv(STRINGDB_API_KEY = "key1")
    string_db$get_values_ranks_enrichment(c("9606.A" = 1.5, "9606.B" = -2), quiet = TRUE)
  })
  checkEquals(2, nrow(results))
  checkEquals(STRINGdb:::VALUES_RANKS_RESULT_COLUMNS, colnames(results))
  checkEquals("9606.A\t1.5\n9606.B\t-2", log$requests[[1]]$params$identifiers)

}


test_values_ranks_bundled_sample <- function() {

  # the vignette's sample has the current format of the results: same columns, same types
  sample_rows <- readRDS(system.file("extdata", "values_ranks_example.rds", package = "STRINGdb"))
  checkTrue(is.data.frame(sample_rows) && nrow(sample_rows) > 0)
  checkEquals(STRINGdb:::VALUES_RANKS_RESULT_COLUMNS, colnames(sample_rows))
  current <- STRINGdb:::read_values_ranks_results(values_ranks_tsv, "job123")
  checkEquals(lapply(current, class), lapply(sample_rows, class))
  checkTrue(all(lengths(sample_rows$inputValues) == sample_rows$number_of_genes))
  checkTrue(all(sample_rows$category %in% c(STRINGdb:::VALUES_RANKS_CATEGORY_CODES, STRINGdb:::VALUES_RANKS_PHENOTYPE_PREFIXES)))

}


test_values_ranks_input_checks <- function() {

  # string_ids and values are paired by position: values named by STRING identifiers in another
  # order stop; names in the same order, or names that are other identifiers, do not
  ids <- c("9606.A", "9606.B", "9606.C")
  error_message <- tryCatch(
    STRINGdb:::values_ranks_input_of(ids, c("9606.C" = 1, "9606.B" = 2, "9606.A" = 3)),
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("paired by position", error_message, fixed = TRUE))
  reordered_table <- matrix(1:6, nrow = 3, dimnames = list(rev(ids), c("x", "y")))
  checkException(STRINGdb:::values_ranks_input_of(ids, reordered_table), silent = TRUE)
  checkEquals(ids, STRINGdb:::values_ranks_input_of(ids, c("9606.A" = 1, "9606.B" = 2, "9606.C" = 3))$string_ids)
  checkEquals(ids, STRINGdb:::values_ranks_input_of(ids, c(TP53 = 1, MYC = 2, EGFR = 3))$string_ids)
  checkEquals(ids, STRINGdb:::values_ranks_input_of(ids, data.frame(x = 1:3, y = 3:1))$string_ids)

  # values that are all NA say so
  error_message <- tryCatch(STRINGdb:::values_ranks_input_of(values = c(a = NA, b = NA)), error = function(e) conditionMessage(e))
  checkTrue(grepl("all NA", error_message, fixed = TRUE))
  error_message <- tryCatch(STRINGdb:::prepare_values_ranks_input(ids, c(NA, NA, NA)), error = function(e) conditionMessage(e))
  checkTrue(grepl("all NA", error_message, fixed = TRUE))

  # infinite values (e.g. -log10 of a p-value of 0) are left out with a warning, also with
  # quiet = TRUE, for a vector and for a table; NA values are left out without one
  warned <- character(0)
  remember_warning <- function(w) {
    warned <<- c(warned, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
  prepared <- withCallingHandlers(STRINGdb:::prepare_values_ranks_experiments(ids, c(Inf, -Inf, 1)), warning = remember_warning)
  checkEquals(1, prepared$experiments[[1]]$n_submitted)
  checkEquals(1, length(warned))
  checkTrue(grepl("2 infinite values were left out, with their proteins", warned, fixed = TRUE))

  warned <- character(0)
  table_values <- data.frame(x = c(Inf, 1, 2), y = c(1, 2, 3))
  withCallingHandlers(STRINGdb:::prepare_values_ranks_experiments(ids, table_values), warning = remember_warning)
  checkEquals(1, length(warned))
  checkTrue(grepl("1 infinite value was left out in 1 of 2 experiments, with its protein", warned, fixed = TRUE))

  warned <- character(0)
  withCallingHandlers(STRINGdb:::prepare_values_ranks_experiments(ids, c(NA, 1, 2)), warning = remember_warning)
  checkEquals(0, length(warned))

  # quiet is TRUE or FALSE
  checkException(STRINGdb:::check_values_ranks_quiet(NA), silent = TRUE)
  checkException(STRINGdb:::check_values_ranks_quiet("yes"), silent = TRUE)
  checkTrue(is.null(STRINGdb:::check_values_ranks_quiet(FALSE)))

}


test_values_ranks_needs_string_12 <- function() {

  # STRING's values/ranks API and its API keys exist since version 12.0: with an older version
  # the methods stop before any request
  requests <- 0
  original_post <- get("post_string_json", envir = asNamespace("STRINGdb"))
  assignInNamespace("post_string_json", function(url_str, params, timeout_seconds = 60) {
    requests <<- requests + 1
    stop("no request expected")
  }, ns = "STRINGdb")
  on.exit(assignInNamespace("post_string_json", original_post, ns = "STRINGdb"), add = TRUE)

  string_db <- suppressWarnings(suppressMessages(STRINGdb$new(version = "11.5", species = 9606, score_threshold = 400)))
  string_db$stable_url <- "https://127.0.0.1:9"
  with_temporary_key_file({
    calls <- list(
      function() string_db$get_values_ranks_enrichment(c("9606.A", "9606.B"), c(1, -1)),
      function() string_db$submit_values_ranks_enrichment(c("9606.A", "9606.B"), c(1, -1)),
      function() string_db$get_values_ranks_status("job1"),
      function() string_db$get_values_ranks_results("job1"),
      function() string_db$get_api_key()
    )
    for (call in calls) {
      error_message <- tryCatch(call(), error = function(e) conditionMessage(e))
      checkTrue(grepl("needs STRING version 12.0 or later; this object uses version 11.5", error_message, fixed = TRUE))
    }
  })
  checkEquals(0, requests)

}


test_values_ranks_timeout_per_job <- function() {

  # the timeout is per job and starts at the job's submission: STRING takes one unfinished job at
  # a time, so the three jobs are submitted one after another. The run takes longer than the
  # timeout, but each job finishes within its own.
  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
  waits <- fake_waits()
  run <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, list(key = "key1", source = "environment"), options,
    timeout_seconds = 20, quiet = TRUE, send = fake_values_ranks_server(rounds = 2, limit = 1),
    fetch = fake_download(), sleep = waits$sleep, clock = waits$clock
  )
  checkEquals(c("A", "B", "C"), sort(names(run$results)))
  checkEquals(0, length(run$pending))
  checkTrue(!run$timed_out)
  checkTrue(waits$now > 20)

}


# The messages of a call, without the line break that message() adds, and its value.
messages_of <- function(run) {
  messages <- character(0)
  value <- withCallingHandlers(
    run(),
    message = function(m) {
      messages <<- c(messages, sub("\n$", "", conditionMessage(m)))
      invokeRestart("muffleMessage")
    }
  )
  return(list(value = value, messages = messages))
}


test_values_ranks_progress_labels <- function() {

  # the values of one experiment are named as typed: a variable or a column, otherwise "values"
  checkEquals("ranked", STRINGdb:::values_ranks_label_of(quote(ranked)))
  checkEquals("mapped$logFC", STRINGdb:::values_ranks_label_of(quote(mapped$logFC)))
  checkEquals("mapped[[\"logFC\"]]", STRINGdb:::values_ranks_label_of(quote(mapped[["logFC"]])))
  checkEquals("results$contrast$logFC", STRINGdb:::values_ranks_label_of(quote(results$contrast$logFC)))
  checkEquals("values", STRINGdb:::values_ranks_label_of(quote(sign(x) * -log10(p))))
  checkEquals("values", STRINGdb:::values_ranks_label_of(quote(mapped[[2]])))

  # an experiment is named by its name; a job known only by its id, by "Job" and the id
  checkEquals("Experiment logFC", STRINGdb:::values_ranks_progress_label("logFC"))
  checkEquals("Experiment logFC", STRINGdb:::values_ranks_progress_label("logFC", "job1"))
  checkEquals("Job job1", STRINGdb:::values_ranks_progress_label("job1", "job1"))
  checkEquals("Experiment A", STRINGdb:::values_ranks_progress_label("A", NA))

  # a message names at most 10 experiments, and says how many more there are
  checkEquals("A, B", STRINGdb:::values_ranks_names_text(c("A", "B")))
  checkEquals(
    paste(paste(LETTERS[1:10], collapse = ", "), "and 2 more"),
    STRINGdb:::values_ranks_names_text(LETTERS[1:12])
  )

  # the progress of a wait starts with the experiment's label, or with the job id by default
  progress_records <- list(
    status_record("running", "computing statistical enrichment p-values ..."),
    status_record("success", "Job finished", "https://example.org/download")
  )
  waited <- messages_of(function() {
    STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, send = fake_string_json(progress_records),
                                         sleep = fake_waits()$sleep, label = "Experiment mapped$logFC")
  })
  checkEquals("Experiment mapped$logFC: running (computing statistical enrichment p-values ...)", waited$messages)
  waited <- messages_of(function() {
    STRINGdb:::wait_for_values_ranks_job("u", "key1", "job123", 7200, send = fake_string_json(progress_records),
                                         sleep = fake_waits()$sleep)
  })
  checkEquals("Job job123: running (computing statistical enrichment p-values ...)", waited$messages)

}


test_values_ranks_progress_messages_one_experiment <- function() {

  # the methods name a vector of values as the caller typed it; the job id is given once, when the
  # job is submitted. The transport functions are replaced for this test (and restored at its end).
  fake_post <- function(url_str, params, timeout_seconds = 60) {
    if (grepl("valuesranks_enrichment_submit", url_str, fixed = TRUE)) {
      return(list(list(job_id = "job123", status = "submitted", message = "")))
    }
    return(status_record("success", "Job finished", "https://example.org/download"))
  }
  original_post <- get("post_string_json", envir = asNamespace("STRINGdb"))
  original_get_text <- get("get_string_text", envir = asNamespace("STRINGdb"))
  assignInNamespace("post_string_json", fake_post, ns = "STRINGdb")
  assignInNamespace("get_string_text", function(url_str, query = NULL, timeout_seconds = 300) values_ranks_tsv, ns = "STRINGdb")
  on.exit(assignInNamespace("post_string_json", original_post, ns = "STRINGdb"), add = TRUE)
  on.exit(assignInNamespace("get_string_text", original_get_text, ns = "STRINGdb"), add = TRUE)

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$stable_url <- "https://127.0.0.1:9"
  mapped <- data.frame(STRING_id = c("9606.A", "9606.B"), logFC = c(1.5, -2), stringsAsFactors = FALSE)
  ranked <- setNames(mapped$logFC, mapped$STRING_id)

  with_temporary_key_file({
    Sys.setenv(STRINGDB_API_KEY = "key1")

    run <- messages_of(function() string_db$get_values_ranks_enrichment(mapped$STRING_id, mapped$logFC))
    checkEquals(2, nrow(run$value))
    checkTrue("Experiment mapped$logFC: submitted 2 proteins to STRING's values/ranks enrichment (job job123)." %in% run$messages)
    checkTrue("Experiment mapped$logFC: 2 enriched terms." %in% run$messages)
    checkTrue(!any(grepl("STRING job", run$messages, fixed = TRUE)))

    # the values alone, named by STRING identifiers, and a calculation
    run <- messages_of(function() string_db$get_values_ranks_enrichment(ranked, max_fdr = 0.01))
    checkTrue("Experiment ranked: 2 enriched terms." %in% run$messages)
    run <- messages_of(function() string_db$get_values_ranks_enrichment(mapped$STRING_id, mapped$logFC * 2))
    checkTrue("Experiment values: 2 enriched terms." %in% run$messages)

    run <- messages_of(function() string_db$submit_values_ranks_enrichment(mapped$STRING_id, mapped$logFC))
    checkEquals("job123", run$value)
    checkTrue(any(startsWith(run$messages, "Experiment mapped$logFC: submitted 2 proteins to STRING's values/ranks enrichment (job job123).")))

    # the results of a job known only by its id are named by the id
    run <- messages_of(function() string_db$get_values_ranks_results("job123"))
    checkTrue("Job job123: 2 enriched terms." %in% run$messages)
  })

}


test_values_ranks_progress_messages_table <- function() {

  options <- STRINGdb:::normalize_values_ranks_options(0.01, "descending")
  key_info <- list(key = "key1", source = "environment")

  # the experiments of a table are named when they are submitted, and each when it has finished
  # or failed, with how many have finished
  waits <- fake_waits()
  run <- messages_of(function() {
    STRINGdb:::run_values_ranks_jobs(
      "https://s", 9606, three_experiments()$experiments, key_info, options,
      send = fake_values_ranks_server(rounds = 1, failing = "job2"), fetch = fake_download(),
      sleep = waits$sleep, clock = waits$clock
    )
  })
  checkTrue("Submitted 3 experiments to STRING's values/ranks enrichment: A, B, C." %in% run$messages)
  checkTrue(any(grepl("^Experiment A: 2 enriched terms \\([123] of 3 experiments finished\\)\\.$", run$messages)))
  checkTrue(any(grepl("^Experiment B failed: STRING's values/ranks job job2 failed: Error. Something went wrong \\([123] of 3 experiments finished\\)\\.$", run$messages)))
  checkTrue(any(grepl("(3 of 3 experiments finished).", run$messages, fixed = TRUE)))
  checkTrue(!any(grepl("values/ranks jobs:", run$messages, fixed = TRUE)))

  # experiments that STRING cannot take now are reported once, and named when they are submitted
  waits <- fake_waits()
  run <- messages_of(function() {
    STRINGdb:::run_values_ranks_jobs(
      "https://s", 9606, three_experiments()$experiments, key_info, options,
      send = fake_values_ranks_server(rounds = 1, limit = 2), fetch = fake_download(),
      sleep = waits$sleep, clock = waits$clock
    )
  })
  checkTrue("Submitted 2 experiments to STRING's values/ranks enrichment: A, B." %in% run$messages)
  checkEquals(1, sum(grepl("^1 more experiment waits to be submitted \\(Job submission limit reached", run$messages)))
  checkTrue("Submitted 1 experiment to STRING's values/ranks enrichment: C." %in% run$messages)
  checkEquals(3, sum(grepl("of 3 experiments finished).", run$messages, fixed = TRUE)))

  # jobs known only by their ids (named by themselves) are named "Job" in the messages
  log <- new.env()
  server <- fake_values_ranks_server(rounds = 1, log = log)
  submitted <- STRINGdb:::run_values_ranks_jobs(
    "https://s", 9606, three_experiments()$experiments, key_info, options, wait = FALSE, quiet = TRUE,
    send = server, fetch = fake_download()
  )
  job_ids <- unname(submitted$job_ids)
  names(job_ids) <- job_ids
  waits <- fake_waits()
  run <- messages_of(function() {
    STRINGdb:::run_values_ranks_jobs(
      "https://s", 9606, list(), key_info, job_ids = job_ids, job_ids_known = TRUE,
      send = server, fetch = fake_download(), sleep = waits$sleep, clock = waits$clock
    )
  })
  checkTrue(any(grepl("^Job job1: 2 enriched terms \\([123] of 3 jobs finished\\)\\.$", run$messages)))

}
