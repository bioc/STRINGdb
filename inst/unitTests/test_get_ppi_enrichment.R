# The test calls STRING; it is deactivated when STRING cannot be reached (helper_live_string.R).
source(system.file("unitTests", "helper_live_string.R", package = "STRINGdb"), local = TRUE)


test_get_ppi_enrichment <- function() {

  with_live_string({

    string_db <- STRINGdb$new( version="12.5", species=9606, score_threshold=400)
    data(diff_exp_example1)
    example = string_db$map( diff_exp_example1, "gene", removeUnmappedRows = TRUE )
    hits = example$STRING_id[1:20]
    string_db$set_background(hits)
    ppi_enrichment = string_db$get_ppi_enrichment( hits )
    checkTrue( ppi_enrichment$enrichment >= 0.2 )

  })

}
