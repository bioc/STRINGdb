test_homology_graph_bitscore <- function() {

  # get_homology_graph keeps the pairs whose bitscore (column 4 of STRING's homology file) reaches the
  # threshold; column 9 is the length of the second protein. A small homology file, named like STRING's
  # download, is placed in the object's input_directory, so nothing is downloaded.
  input_directory <- tempfile("stringdb_homology")
  dir.create(input_directory)
  on.exit(unlink(input_directory, recursive = TRUE), add = TRUE)

  homology_lines <- c(
    "#string_protein_1\tstring_protein_id_2\tspecies_2\tbitscore\tstart_1\tend_1\tstart_2\tend_2\tsize_2",
    # similar proteins of short length: kept by bitscore, dropped by the old length filter
    "9606.A\t9606.B\t9606\t200.5\t1\t50\t1\t50\t50",
    # dissimilar proteins of large length: dropped by bitscore, kept by the old length filter
    "9606.A\t9606.C\t9606\t55.2\t1\t300\t1\t300\t300",
    "9606.B\t9606.C\t9606\t80.0\t1\t80\t1\t80\t80"
  )
  homology_file <- gzfile(file.path(input_directory, "9606.protein.homology.v12.5.txt.gz"), "w")
  writeLines(homology_lines, homology_file)
  close(homology_file)

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400, input_directory = input_directory)

  homology_graph <- string_db$get_homology_graph(60)
  edges <- igraph::as_data_frame(homology_graph)
  checkEquals(c("9606.A-9606.B", "9606.B-9606.C"), sort(paste(edges$from, edges$to, sep = "-")))

  # a threshold above every bitscore keeps no pair
  checkEquals(0, igraph::ecount(string_db$get_homology_graph(1000)))

}
