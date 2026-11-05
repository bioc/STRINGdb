# The test downloads STRING files; it is deactivated when STRING cannot be reached (helper_live_string.R).
source(system.file("unitTests", "helper_live_string.R", package = "STRINGdb"), local = TRUE)


test_get_clusters <- function(){

  with_live_string({

    string_db <- STRINGdb$new( version="12.5", species=511145, score_threshold=800 )

    proteins <- string_db$get_proteins()
    hits <- proteins$protein_external_id[1:50]
    clusters <- string_db$get_clusters(hits)
    checkTrue(length(clusters) > 1)

  })

}
