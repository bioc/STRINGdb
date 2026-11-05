# Makes inst/extdata/values_ranks_example.rds, the sample of a values/ranks result that the
# vignette shows (its values/ranks code cannot run while the vignette is built, as it submits jobs
# to STRING's queue). It runs the vignette's example on STRING and keeps six terms of the result,
# the two most significant ones of each direction (top, bottom, both ends), without the attributes of
# the job (job_id, page_url, ...), whose results expire on STRING.
# Run it from the package's directory with a network connection; it uses or requests the STRING API
# key of this computer (see get_api_key).

library(STRINGdb)

string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400,
                          network_type = "full", link_data = "detailed", input_directory = "")
data(diff_exp_example1)
example1_mapped <- string_db$map(diff_exp_example1, "gene", removeUnmappedRows = TRUE)

ranked_enrichment <- string_db$get_values_ranks_enrichment(example1_mapped$STRING_id, example1_mapped$logFC,
                                                           max_fdr = 0.01)

# the two most significant terms of each direction
sample_rows <- do.call(rbind, lapply(c("top", "bottom", "both ends"), function(direction) {
  terms <- ranked_enrichment[ranked_enrichment$direction == direction, ]
  head(terms[order(terms$fdr), ], 2)
}))
rownames(sample_rows) <- NULL
attributes(sample_rows) <- attributes(sample_rows)[c("names", "row.names", "class")]

dir.create(file.path("inst", "extdata"), showWarnings = FALSE)
saveRDS(sample_rows, file.path("inst", "extdata", "values_ranks_example.rds"))
