# Offline tests of the identifier mapping (map, mp and multi_map_df). The alias
# tables are small stand-ins for the ones that get_aliases() downloads.

# Three human proteins as STRING lists their aliases. One alias contains the
# hyphen U+2010, which the normalization of the aliases used to turn into NA,
# so that every NA identifier was mapped to that protein.
mapping_alias_table <- function() {
  data.frame(
    STRING_id = c("9606.ENSP00000269305", "9606.ENSP00000258149", "9606.ENSP00000222345", "9606.ENSP00000222345"),
    alias = c("TP53", "MDM2", "SIPA1L3", "spine‐associated RapGAP 3"),
    sources = c("preferred_name", "preferred_name", "preferred_name", "UniProt_DE_RecName_Full"),
    stringsAsFactors = FALSE
  )
}


test_map_missing_identifiers_stay_unmapped <- function() {

  aliases <- STRINGdb:::prioritize_aliases_by_source(mapping_alias_table())
  input <- data.frame(
    gene = c("TP53", NA, "", "spine‐associated RapGAP 3"),
    logFC = c(1.2, 0.4, -0.9, 2.0),
    stringsAsFactors = FALSE
  )

  mapped <- STRINGdb:::multi_map_df(input, aliases, "gene", "alias", "STRING_id")

  checkEquals(c("9606.ENSP00000269305", NA, NA, "9606.ENSP00000222345"), mapped$STRING_id)
  checkEquals(input$gene, mapped$gene)
  checkEquals(input$logFC, mapped$logFC)

}


test_map_several_id_columns <- function() {

  aliases <- STRINGdb:::prioritize_aliases_by_source(mapping_alias_table())
  input <- data.frame(
    gene = c("tp53", "unknown1", "mdm2"),
    alternative = c(NA, "sipa1l3", NA),
    logFC = c(1.5, -2.0, 0.7),
    stringsAsFactors = FALSE
  )

  mapped <- STRINGdb:::multi_map_df(input, aliases, c("gene", "alternative"), "alias", "STRING_id")

  # Every row keeps its own identifiers and values, in the order of the input; the
  # identifiers used to be written back by position after the rows were reordered.
  checkEquals(c("gene", "alternative", "logFC", "STRING_id"), colnames(mapped))
  checkEquals(input$gene, mapped$gene)
  checkEquals(input$alternative, mapped$alternative)
  checkEquals(input$logFC, mapped$logFC)
  checkEquals(c("9606.ENSP00000269305", "9606.ENSP00000222345", "9606.ENSP00000258149"), mapped$STRING_id)

}


test_map_column_order <- function() {

  # As before, the identifier column comes first and STRING_id last; other packages
  # rename the columns of map() by position.
  aliases <- STRINGdb:::prioritize_aliases_by_source(mapping_alias_table())
  input <- data.frame(pvalue = c(0.01, 0.2), logFC = c(1.5, 0.7), gene = c("TP53", "MDM2"), stringsAsFactors = FALSE)

  mapped <- STRINGdb:::multi_map_df(input, aliases, "gene", "alias", "STRING_id")

  checkEquals(c("gene", "pvalue", "logFC", "STRING_id"), colnames(mapped))
  checkEquals(c(0.01, 0.2), mapped$pvalue)
  checkEquals(c("9606.ENSP00000269305", "9606.ENSP00000258149"), mapped$STRING_id)

}


test_map_matrix_input <- function() {

  # A matrix is mapped as a data frame, as in STRINGdb 2.24 and earlier.
  aliases <- STRINGdb:::prioritize_aliases_by_source(mapping_alias_table())
  input <- cbind(gene = c("tp53", "unknown1", "MDM2"), logFC = c("1.5", "-2", "0.1"))

  mapped <- STRINGdb:::multi_map_df(input, aliases, "gene", "alias", "STRING_id")

  checkTrue(is.data.frame(mapped))
  checkEquals(c("gene", "logFC", "STRING_id"), colnames(mapped))
  checkEquals(c("tp53", "unknown1", "MDM2"), mapped$gene)
  checkEquals(c("1.5", "-2", "0.1"), mapped$logFC)
  checkEquals(c("9606.ENSP00000269305", NA, "9606.ENSP00000258149"), mapped$STRING_id)

}


test_map_methods_offline <- function() {

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)

  # With the aliases already loaded, map() and mp() do not download the alias file.
  string_db$aliases_tf <- STRINGdb:::prioritize_aliases_by_source(mapping_alias_table())
  input <- data.frame(gene = c("TP53", NA, "unknown1", "MDM2"), logFC = c(1.2, 0.4, -0.9, 0.7), stringsAsFactors = FALSE)

  mapped <- string_db$map(input, "gene", quiet = TRUE)
  checkEquals(c("9606.ENSP00000269305", NA, NA, "9606.ENSP00000258149"), mapped$STRING_id)

  mapped <- string_db$map(input, "gene", removeUnmappedRows = TRUE, quiet = TRUE)
  checkEquals(c("TP53", "MDM2"), mapped$gene)

  checkEquals("9606.ENSP00000269305", string_db$mp(c("tp53", NA)))

  # takeFirst = FALSE: an alias of two proteins repeats its row, and the identifiers
  # keep their spelling (they used to come back upper-cased).
  string_db$aliases_all <- data.frame(
    STRING_id = c("9606.ENSP00000269305", "9606.ENSP00000258149", "9606.ENSP00000222345"),
    alias = c("TP53", "MDM2", "MDM2"),
    stringsAsFactors = FALSE
  )
  mapped <- string_db$map(data.frame(gene = c("tp53", "mdm2"), stringsAsFactors = FALSE), "gene", takeFirst = FALSE, quiet = TRUE)
  checkEquals(c("tp53", "mdm2", "mdm2"), mapped$gene)
  checkEquals("9606.ENSP00000269305", mapped$STRING_id[1])
  checkEquals(c("9606.ENSP00000222345", "9606.ENSP00000258149"), sort(mapped$STRING_id[2:3]))

}


test_get_summary_default_score <- function() {

  # get_summary(string_ids), as documented, uses the score_threshold of the object.
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  summary_arguments <- formals(string_db$get_summary)
  checkTrue("required_score" %in% names(summary_arguments))
  checkTrue(is.null(summary_arguments[["required_score"]]))

}
