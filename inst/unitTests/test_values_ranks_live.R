# Live test of STRING's values/ranks enrichment: a real job on STRING's queue, which can take
# many minutes. It runs only when asked for, with STRINGDB_TEST_VALUES_RANKS=true and an API key in
# STRINGDB_API_KEY (see get_api_key), so that a check on a computer that has a key does not submit
# a job, and that the test never requests or saves a key. It is deactivated otherwise, e.g. on the
# Bioconductor build machines, and when STRING cannot be reached (helper_live_string.R).
source(system.file("unitTests", "helper_live_string.R", package = "STRINGdb"), local = TRUE)


test_values_ranks_live <- function() {

  if (!identical(tolower(Sys.getenv("STRINGDB_TEST_VALUES_RANKS")), "true")) {
    DEACTIVATED("set STRINGDB_TEST_VALUES_RANKS=true (and STRINGDB_API_KEY) to test STRING's values/ranks enrichment")
  }
  if (nchar(Sys.getenv("STRINGDB_API_KEY")) == 0) {
    DEACTIVATED("STRINGDB_API_KEY is not set, so STRING's values/ranks enrichment is not tested")
  }

  with_live_string({

    string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
    data(diff_exp_example1, package = "STRINGdb", envir = environment())
    mapped <- string_db$map(diff_exp_example1, "gene", removeUnmappedRows = TRUE, quiet = TRUE)

    # the values alone, named by STRING identifiers
    ranked <- setNames(mapped$logFC, mapped$STRING_id)
    results <- string_db$get_values_ranks_enrichment(ranked, timeout = 1800, quiet = TRUE)
    checkEquals(STRINGdb:::VALUES_RANKS_RESULT_COLUMNS, colnames(results))
    checkTrue(nrow(results) > 0)
    checkTrue(is.integer(results$number_of_genes))
    checkTrue(all(results$fdr <= 0.05))
    checkTrue(all(results$direction %in% c("top", "bottom", "both ends")))
    # the category codes of get_enrichment, and one value and rank for each protein of a term
    checkTrue("Process" %in% results$category)
    checkTrue(all(lengths(results$inputValues) == results$number_of_genes))
    checkTrue(all(lengths(results$inputRanks) == results$number_of_genes))
    checkTrue(nchar(attr(results, "job_id")) > 0)

    # the finished job's results can be fetched again by its job_id
    again <- string_db$get_values_ranks_results(attr(results, "job_id"), timeout = 600, quiet = TRUE)
    checkEquals(nrow(results), nrow(again))

  })

}
