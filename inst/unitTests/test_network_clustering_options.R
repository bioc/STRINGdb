test_normalize_network_clustering <- function() {

  # Without an algorithm, no clustering parameter is sent.
  checkEquals(length(STRINGdb:::normalize_network_clustering()), 0)

  # STRING's names of the algorithms, and the defaults of the Clusters tab of the website.
  mcl <- STRINGdb:::normalize_network_clustering("mcl")
  checkEquals(mcl$network_clustering_algorithm, "MCL")
  checkEquals(mcl$network_clustering_parameter_mcl, "3")

  leiden <- STRINGdb:::normalize_network_clustering("Leiden")
  checkEquals(leiden$network_clustering_algorithm, "leiden")
  checkEquals(leiden$network_clustering_parameter_leiden, "1")

  kmeans <- STRINGdb:::normalize_network_clustering(
    "kmeans", 5, cluster_aware_layout = TRUE, edge_visibility = "Dotted",
    show_cluster_names = TRUE, cluster_name_size = 2.5
  )
  checkEquals(kmeans$network_clustering_algorithm, "kmeans")
  checkEquals(kmeans$network_clustering_parameter_kmeans, "5")
  checkTrue(identical(kmeans$cluster_aware_layout, 1L))
  checkEquals(kmeans$network_clustering_edge_visibility, "dotted")
  checkTrue(identical(kmeans$show_cluster_names, 1L))
  checkEquals(kmeans$cluster_name_size, "2.5")

  # Options without an algorithm, unknown algorithms and values outside the ranges stop with an error.
  fails <- function(expression) {
    tryCatch(
      {
        force(expression)
        FALSE
      },
      error = function(e) TRUE
    )
  }
  checkTrue(fails(STRINGdb:::normalize_network_clustering(NULL, 3)))
  checkTrue(fails(STRINGdb:::normalize_network_clustering(show_cluster_names = TRUE)))
  checkTrue(fails(STRINGdb:::normalize_network_clustering("louvain")))
  checkTrue(fails(STRINGdb:::normalize_network_clustering("mcl", 1)))
  checkTrue(fails(STRINGdb:::normalize_network_clustering("leiden", 11)))
  checkTrue(fails(STRINGdb:::normalize_network_clustering("kmeans", 2.5)))
  checkTrue(fails(STRINGdb:::normalize_network_clustering("kmeans", edge_visibility = "dashed")))
  checkTrue(fails(STRINGdb:::normalize_network_clustering("kmeans", cluster_name_size = 7)))

}


test_get_png_clustering <- function() {

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)

  # TP53, MDM2, CDKN1A, ATM, EGFR, KRAS, GRB2, SOS1, PTEN and AKT1: Leiden splits them into named clusters
  string_ids <- c(
    "9606.ENSP00000269305", "9606.ENSP00000258149", "9606.ENSP00000384849", "9606.ENSP00000278616",
    "9606.ENSP00000275493", "9606.ENSP00000256078", "9606.ENSP00000376345", "9606.ENSP00000384675",
    "9606.ENSP00000361021", "9606.ENSP00000451828"
  )

  svg <- string_db$get_png(
    string_ids, output_format = "svg", network_clustering_algorithm = "leiden",
    show_cluster_names = TRUE, cluster_name_size = 2
  )
  checkTrue(grepl("nwclusterlabel", svg, fixed = TRUE))

  png <- string_db$get_png(string_ids, network_clustering_algorithm = "mcl", cluster_aware_layout = TRUE)
  checkTrue(length(dim(png)) == 3)

  # A picture that STRING refuses stops with STRING's explanation.
  error_message <- tryCatch(
    {
      string_db$get_png(network_term_id = "GO:9999999", output_format = "svg")
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  checkTrue(!is.null(error_message))
  checkTrue(grepl("No proteins of Homo sapiens are annotated with GO:9999999", error_message, fixed = TRUE))

}
