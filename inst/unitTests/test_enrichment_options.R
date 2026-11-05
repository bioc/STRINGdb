test_enrichment_score_params <- function() {

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

  # add_scores = FALSE (the default of get_enrichment) sends nothing, so that the request is the one sent before.
  for (no_scores in list(FALSE, 0)) {
    result <- call_and_note_warning(STRINGdb:::enrichment_score_params(no_scores, "12.5"))
    checkEquals(length(result$value), 0)
    checkTrue(!result$warned)
  }

  # add_scores = TRUE sends add_scores=1, which asks STRING for the strength and signal columns.
  for (scores in list(TRUE, 1)) {
    result <- call_and_note_warning(STRINGdb:::enrichment_score_params(scores, "12.5"))
    checkTrue(identical(result$value, list(add_scores = 1L)))
    checkTrue(!result$warned)
  }

  # Versions older than 12.5 ignore add_scores without a word: it is not sent, with a warning.
  for (old_version in c("12.0", "11.5", "11.0b")) {
    result <- call_and_note_warning(STRINGdb:::enrichment_score_params(TRUE, old_version))
    checkEquals(length(result$value), 0)
    checkTrue(result$warned)
  }

  # Without the option, an older version does not warn.
  result <- call_and_note_warning(STRINGdb:::enrichment_score_params(FALSE, "12.0"))
  checkEquals(length(result$value), 0)
  checkTrue(!result$warned)

  # Anything else than TRUE/FALSE or 1/0 stops with an error.
  fails <- function(expression) {
    tryCatch(
      {
        force(expression)
        FALSE
      },
      error = function(e) TRUE
    )
  }
  checkTrue(fails(STRINGdb:::enrichment_score_params("yes", "12.5")))
  checkTrue(fails(STRINGdb:::enrichment_score_params(NA, "12.5")))
  checkTrue(fails(STRINGdb:::enrichment_score_params(2, "12.5")))
  checkTrue(fails(STRINGdb:::enrichment_score_params(c(TRUE, FALSE), "12.5")))

}


test_get_enrichment_add_scores_validation <- function() {

  # A known version is created without a request to STRING, and an invalid add_scores stops get_enrichment
  # before its request.
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)

  error_message <- tryCatch(
    {
      string_db$get_enrichment(c("9606.ENSP00000269305", "9606.ENSP00000258149"), add_scores = "yes")
      ""
    },
    error = function(e) conditionMessage(e)
  )
  checkTrue(grepl("add_scores should be TRUE/FALSE or 0/1", error_message, fixed = TRUE))

}


test_get_enrichment_figure_accepted_values <- function() {

  # A known version is created without a request to STRING.
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)

  # TP53 and MDM2
  string_ids <- c("9606.ENSP00000269305", "9606.ENSP00000258149")

  # get_enrichment_figure checks its arguments in turn and prints an ERROR line before it stops, without a request.
  # number_of_term_shown = 0 is checked last, so a call that prints its error has passed the checks of category,
  # color_palette, x_axis and sort_by. Returns the ERROR line printed.
  figure_error <- function(...) {
    error_message <- ""
    output <- capture.output(
      stopped <- tryCatch(
        {
          string_db$get_enrichment_figure(string_ids, number_of_term_shown = 0, ...)
          FALSE
        },
        error = function(e) {
          error_message <<- conditionMessage(e)
          TRUE
        }
      )
    )
    checkTrue(stopped)
    # the older checks print their ERROR line, the newer ones stop with it as the error message
    return(paste(c(output, error_message), collapse = " "))
  }
  passes_checks <- function(...) {
    return(grepl("number_of_term_shown", figure_error(...), fixed = TRUE))
  }

  # The categories that STRING draws, among them GWAS and Hallmark.
  categories <- c(
    "Process", "Function", "Component", "Keyword", "KEGG", "RCTM",
    "HPO", "MPO", "DPO", "WPO", "ZPO", "FYPO", "GWAS", "Hallmark",
    "Pfam", "SMART", "InterPro", "PMID", "NetworkNeighborAL",
    "COMPARTMENTS", "TISSUES", "DISEASES", "WikiPathways"
  )
  for (category in categories) {
    checkTrue(passes_checks(category = category))
  }

  palettes <- c("mint_blue", "lime_emerald", "green_blue", "peach_purple", "straw_navy", "yellow_pink", "red_blue")
  for (color_palette in palettes) {
    checkTrue(passes_checks(color_palette = color_palette))
  }

  # "count" is STRING's other name for "gene_count", for the x-axis and the sort order.
  axis_variables <- c("signal", "strength", "FDR", "gene_count", "count")
  for (axis_variable in axis_variables) {
    checkTrue(passes_checks(x_axis = axis_variable))
    checkTrue(passes_checks(sort_by = axis_variable))
  }

  # STRING silently draws its default for any other value, so a misspelt value still stops with an error.
  checkTrue(grepl("category", figure_error(category = "gwas"), fixed = TRUE))
  checkTrue(grepl("category", figure_error(category = "Hallmarks"), fixed = TRUE))
  checkTrue(grepl("color_palette", figure_error(color_palette = "blue_red"), fixed = TRUE))
  checkTrue(grepl("x_axis", figure_error(x_axis = "counts"), fixed = TRUE))
  checkTrue(grepl("sort_by", figure_error(sort_by = "gene count"), fixed = TRUE))

}


test_get_enrichment_figure_values_needing_version <- function() {

  # TP53 and MDM2
  string_ids <- c("9606.ENSP00000269305", "9606.ENSP00000258149")

  # As in test_get_enrichment_figure_accepted_values: number_of_term_shown = 0 is checked last, so a call whose
  # ERROR line names number_of_term_shown has passed the checks of category, color_palette, x_axis and sort_by.
  # Returns the ERROR line, printed or as the error message; the method stops before any request.
  figure_error <- function(string_db, ...) {
    error_message <- ""
    output <- capture.output(
      stopped <- tryCatch(
        {
          string_db$get_enrichment_figure(string_ids, number_of_term_shown = 0, ...)
          FALSE
        },
        error = function(e) {
          error_message <<- conditionMessage(e)
          TRUE
        }
      )
    )
    checkTrue(stopped)
    # the older checks print their ERROR line, the newer ones stop with it as the error message
    return(paste(c(output, error_message), collapse = " "))
  }
  passes_checks <- function(string_db, ...) {
    return(grepl("number_of_term_shown", figure_error(string_db, ...), fixed = TRUE))
  }
  needs_version <- function(string_db, ...) {
    return(grepl("needs STRING version 12.5 or later", figure_error(string_db, ...), fixed = TRUE))
  }

  # Known versions are created without a request to STRING.
  string_db_12_0 <- STRINGdb$new(version = "12.0", species = 9606, score_threshold = 400)
  string_db_12_5 <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)

  # GWAS, Hallmark, red_blue and count are new in STRING 12.5. STRING 12.0 does not refuse them but draws its default
  # figure (GO Process, mint_blue, signal), so with version 12.0 they stop with an error that names the version.
  checkTrue(needs_version(string_db_12_0, category = "GWAS"))
  checkTrue(needs_version(string_db_12_0, category = "Hallmark"))
  checkTrue(needs_version(string_db_12_0, color_palette = "red_blue"))
  checkTrue(needs_version(string_db_12_0, x_axis = "count"))
  checkTrue(grepl("category 'Hallmark'", figure_error(string_db_12_0, category = "Hallmark"), fixed = TRUE))

  # With version 12.5 they pass.
  checkTrue(passes_checks(string_db_12_5, category = "GWAS"))
  checkTrue(passes_checks(string_db_12_5, category = "Hallmark"))
  checkTrue(passes_checks(string_db_12_5, color_palette = "red_blue"))
  checkTrue(passes_checks(string_db_12_5, x_axis = "count"))

  # The values valid before still pass with version 12.0.
  categories_before <- c(
    "Process", "Function", "Component", "Keyword", "KEGG", "RCTM",
    "HPO", "MPO", "DPO", "WPO", "ZPO", "FYPO", "Pfam", "SMART",
    "InterPro", "PMID", "NetworkNeighborAL", "COMPARTMENTS",
    "TISSUES", "DISEASES", "WikiPathways"
  )
  for (category in categories_before) {
    checkTrue(passes_checks(string_db_12_0, category = category))
  }
  palettes_before <- c("mint_blue", "lime_emerald", "green_blue", "peach_purple", "straw_navy", "yellow_pink")
  for (color_palette in palettes_before) {
    checkTrue(passes_checks(string_db_12_0, color_palette = color_palette))
  }
  for (x_axis in c("signal", "strength", "FDR", "gene_count")) {
    checkTrue(passes_checks(string_db_12_0, x_axis = x_axis))
  }

  # sort_by is not sent to version 12.0 at all (it is left out with a warning, as graph_type and the filters), so
  # sort_by = "count" passes the check there.
  checkTrue(passes_checks(string_db_12_0, sort_by = "count"))

  # A misspelt value still gets the list of valid values, with any version.
  checkTrue(grepl("category should be one of", figure_error(string_db_12_0, category = "gwas"), fixed = TRUE))

  # The helper itself: version 11.x and 11.0b are older than 12.5 too; a later version passes.
  stops <- function(expression) {
    output <- capture.output(
      stopped <- tryCatch(
        {
          force(expression)
          FALSE
        },
        error = function(e) TRUE
      )
    )
    return(stopped)
  }
  for (old_version in c("12.0", "11.5", "11.0b", "11.0")) {
    checkTrue(stops(STRINGdb:::stop_on_value_needing_version("Hallmark", c("GWAS", "Hallmark"), "category", old_version)))
    checkTrue(!stops(STRINGdb:::stop_on_value_needing_version("KEGG", c("GWAS", "Hallmark"), "category", old_version)))
  }
  for (new_version in c("12.5", "13.0")) {
    checkTrue(!stops(STRINGdb:::stop_on_value_needing_version("Hallmark", c("GWAS", "Hallmark"), "category", new_version)))
  }

  # The helper stops with its explanation as the error message, and prints nothing.
  printed <- capture.output(
    error_message <- tryCatch(
      STRINGdb:::stop_on_value_needing_version("Hallmark", c("GWAS", "Hallmark"), "category", "12.0"),
      error = function(e) conditionMessage(e)
    )
  )
  checkEquals(0, length(printed))
  checkEquals(
    "ERROR: category 'Hallmark' needs STRING version 12.5 or later; version 12.0 would silently use its default instead.",
    error_message
  )

}
