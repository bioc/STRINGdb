test_normalize_enrichment_filters <- function() {

  # Without filters, no filter parameter is sent.
  no_filters <- STRINGdb:::normalize_enrichment_filters()
  checkTrue(all(vapply(no_filters, is.null, logical(1))))

  filters <- STRINGdb:::normalize_enrichment_filters(
    max_fdr = 0.01, min_gene_count = 3, min_strength = 0.5, min_signal = 1, merge_similarity = 0.8
  )
  checkEquals(filters$max_fdr, "0.01")
  checkEquals(filters$min_gene_count, "3")
  checkEquals(filters$min_strength, "0.5")
  checkEquals(filters$min_signal, "1")
  checkEquals(filters$merge_similarity, "0.8")

  # Values that STRING would ignore stop with an error.
  fails <- function(expression) {
    tryCatch(
      {
        force(expression)
        FALSE
      },
      error = function(e) TRUE
    )
  }
  checkTrue(fails(STRINGdb:::normalize_enrichment_filters(max_fdr = 0)))
  checkTrue(fails(STRINGdb:::normalize_enrichment_filters(max_fdr = 1.5)))
  checkTrue(fails(STRINGdb:::normalize_enrichment_filters(min_gene_count = 0)))
  checkTrue(fails(STRINGdb:::normalize_enrichment_filters(min_gene_count = 2.5)))
  checkTrue(fails(STRINGdb:::normalize_enrichment_filters(min_strength = "high")))
  checkTrue(fails(STRINGdb:::normalize_enrichment_filters(merge_similarity = 0)))

}


test_get_enrichment_filters <- function() {

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)

  # TP53, MDM2, CDKN1A, ATM, EGFR, KRAS, GRB2, SOS1, PTEN and AKT1
  string_ids <- c(
    "9606.ENSP00000269305", "9606.ENSP00000258149", "9606.ENSP00000384849", "9606.ENSP00000278616",
    "9606.ENSP00000275493", "9606.ENSP00000256078", "9606.ENSP00000376345", "9606.ENSP00000384675",
    "9606.ENSP00000361021", "9606.ENSP00000451828"
  )

  enrichment <- string_db$get_enrichment(string_ids)
  filtered <- string_db$get_enrichment(string_ids, max_fdr = 0.001, min_gene_count = 3)

  checkTrue(nrow(filtered) > 0)
  checkTrue(nrow(filtered) < nrow(enrichment))
  checkTrue(all(filtered$fdr <= 0.001))
  checkTrue(all(filtered$number_of_genes >= 3))

}


test_get_enrichment_figure_options <- function() {

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)

  # TP53, MDM2, CDKN1A, ATM, EGFR, KRAS, GRB2, SOS1, PTEN and AKT1
  string_ids <- c(
    "9606.ENSP00000269305", "9606.ENSP00000258149", "9606.ENSP00000384849", "9606.ENSP00000278616",
    "9606.ENSP00000275493", "9606.ENSP00000256078", "9606.ENSP00000376345", "9606.ENSP00000384675",
    "9606.ENSP00000361021", "9606.ENSP00000451828"
  )

  figure <- string_db$get_enrichment_figure(
    string_ids, category = "Process", output_format = "svg",
    graph_type = "barplot", sort_by = "FDR", max_fdr = 0.001
  )
  checkTrue(grepl("<svg", figure, fixed = TRUE))

  graph_type_error <- tryCatch(
    {
      string_db$get_enrichment_figure(string_ids, graph_type = "pie")
      FALSE
    },
    error = function(e) TRUE
  )
  checkTrue(graph_type_error)

}
