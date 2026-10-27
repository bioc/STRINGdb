test_options_supported_by_version <- function() {

  # Notes whether a call warned, and muffles the warning.
  call_and_note_warning <- function(expression) {
    warned <- FALSE
    value <- withCallingHandlers(
      expression,
      warning = function(w) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    )
    return(list(value = value, warned = warned))
  }

  options <- list(max_fdr = "0.01", min_gene_count = NULL)

  # From version 12.5 the options given are sent; NULL options are left out.
  result <- call_and_note_warning(STRINGdb:::options_supported_by_version(options, "12.5", "The enrichment filters"))
  checkTrue(identical(result$value, list(max_fdr = "0.01")))
  checkTrue(!result$warned)

  # Older versions ignore them without a word: they are not sent, with a warning.
  for (old_version in c("12.0", "11.5", "11.0b")) {
    result <- call_and_note_warning(STRINGdb:::options_supported_by_version(options, old_version, "The enrichment filters"))
    checkTrue(length(result$value) == 0)
    checkTrue(result$warned)
  }

  # Without options, nothing to warn about.
  result <- call_and_note_warning(STRINGdb:::options_supported_by_version(list(sort_by = NULL), "12.0", "sort_by"))
  checkTrue(length(result$value) == 0)
  checkTrue(!result$warned)

}


test_new_options_with_version_12_0 <- function() {

  # Notes whether a call warned, and muffles the warning.
  call_and_note_warning <- function(expression) {
    warned <- FALSE
    value <- withCallingHandlers(
      expression,
      warning = function(w) {
        warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    )
    return(list(value = value, warned = warned))
  }

  string_db <- STRINGdb$new(version = "12.0", species = 9606, score_threshold = 400)

  # TP53, MDM2, CDKN1A, ATM, EGFR, KRAS, GRB2, SOS1, PTEN and AKT1
  string_ids <- c(
    "9606.ENSP00000269305", "9606.ENSP00000258149", "9606.ENSP00000384849", "9606.ENSP00000278616",
    "9606.ENSP00000275493", "9606.ENSP00000256078", "9606.ENSP00000376345", "9606.ENSP00000384675",
    "9606.ENSP00000361021", "9606.ENSP00000451828"
  )

  # The options of version 12.5 are not sent to version 12.0, with a warning, and the calls still work.

  enrichment <- call_and_note_warning(string_db$get_enrichment(string_ids, max_fdr = 0.001))
  checkTrue(enrichment$warned)
  checkTrue(nrow(enrichment$value) > 0)

  figure <- call_and_note_warning(
    string_db$get_enrichment_figure(string_ids, output_format = "svg", graph_type = "barplot", sort_by = "FDR")
  )
  checkTrue(figure$warned)
  checkTrue(grepl("<svg", figure$value, fixed = TRUE))

  picture <- call_and_note_warning(
    string_db$get_png(string_ids, output_format = "svg", network_clustering_algorithm = "leiden", show_cluster_names = TRUE)
  )
  checkTrue(picture$warned)
  checkTrue(grepl("<svg", picture$value, fixed = TRUE))

  # Calls without these options do not warn.

  figure <- call_and_note_warning(string_db$get_enrichment_figure(string_ids, output_format = "svg"))
  checkTrue(!figure$warned)
  checkTrue(grepl("<svg", figure$value, fixed = TRUE))

}
