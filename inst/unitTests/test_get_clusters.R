# The test downloads STRING files; it is deactivated when STRING cannot be reached (helper_live_string.R).
source(system.file("unitTests", "helper_live_string.R", package = "STRINGdb"), local = TRUE)


test_get_clusters_unknown_algorithm <- function() {

  # An algorithm that is not offered (such as 'spinglass', listed in older help pages) stops with the
  # algorithms that are, before anything is downloaded.
  input_directory <- tempfile("stringdb_clusters")
  dir.create(input_directory)
  on.exit(unlink(input_directory, recursive = TRUE), add = TRUE)
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400, input_directory = input_directory)

  for (algorithm in list("spinglass", "fastgreed", c("fastgreedy", "walktrap"), 1)) {
    error_message <- tryCatch(
      {
        string_db$get_clusters("9606.ENSP00000269305", algorithm = algorithm)
        ""
      },
      error = function(e) conditionMessage(e)
    )
    checkTrue(grepl("algorithm should be one of fastgreedy, walktrap, edge.betweenness", error_message, fixed = TRUE))
  }
  checkEquals(0, length(list.files(input_directory)))

}


test_get_clusters <- function(){

  with_live_string({

    string_db <- STRINGdb$new( version="12.5", species=511145, score_threshold=800 )

    proteins <- string_db$get_proteins()
    hits <- proteins$protein_external_id[1:50]
    clusters <- string_db$get_clusters(hits)
    checkTrue(length(clusters) > 1)

  })

}
