test_regulatory_network <- function() {

  checkTrue(!STRINGdb:::is_regulatory_network_version_supported("12.0"))
  checkTrue(!STRINGdb:::is_regulatory_network_version_supported("11.0b"))
  checkTrue(STRINGdb:::is_regulatory_network_version_supported("12.5"))

  string_db <- STRINGdb$new(
    version = "12.5", species = 9606, score_threshold = 400,
    network_type = "regulatory"
  )

  string_db$proteins <- data.frame(
    protein_external_id = c("9606.A", "9606.B", "9606.C"),
    preferred_name = c("A", "B", "C"),
    protein_size = c(1, 1, 1),
    annotation = c("", "", ""),
    stringsAsFactors = FALSE
  )
  string_db$graph <- igraph::graph.data.frame(
    data.frame(
      from = c("9606.A", "9606.C"),
      to = c("9606.B", "9606.A"),
      combined_score = c(900, 800),
      stringsAsFactors = FALSE
    ),
    directed = TRUE
  )

  checkTrue(STRINGdb:::is_igraph_directed(string_db$graph))
  checkTrue(identical(sort(unname(string_db$get_neighbors("9606.A", mode = "out"))), "9606.B"))
  checkTrue(identical(sort(unname(string_db$get_neighbors("9606.A", mode = "in"))), "9606.C"))

  partners <- string_db$get_interaction_partners("9606.A")
  checkTrue(any(partners$from == "9606.A" & partners$to == "9606.B"))
  checkTrue(any(partners$from == "9606.C" & partners$to == "9606.A"))

}
