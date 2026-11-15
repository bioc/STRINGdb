test_descriptions_keep_hash <- function() {

  # STRING's text answers are read with comment.char = "", so a description that contains '#' is kept
  # whole; read.table() would otherwise cut it at the '#'. postFormSmart is replaced for this test, and
  # restored at its end, so that no request reaches STRING.
  enrichment_answer <- paste(
    "category\tterm\tnumber_of_genes\tnumber_of_genes_in_background\tncbiTaxonId\tinputGenes\tpreferredNames\tp_value\tfdr\tdescription",
    "GWAS\tGSSO:000705\t2\t6\t9606\t9606.ENSP1,9606.ENSP2\tA,B\t0.0001\t0.01\tLanguage #Bulgarian",
    sep = "\n"
  )
  annotation_answer <- "GWAS\tGSSO:000705\t2\t0.33\t9606\t9606.ENSP1,9606.ENSP2\tA,B\tLanguage #Bulgarian"
  description_answer <- paste(
    "primary_description\tsecondary_description\ttertiary_description",
    "DNA repair #1\tCell cycle\tApoptosis",
    sep = "\n"
  )
  answer_for <- function(uri, .params = list(), .opts = list(), .ctype = "text", .stop_on_client_error = FALSE) {
    if (grepl("functional_annotation", uri, fixed = TRUE)) {
      return(annotation_answer)
    }
    if (grepl("geneset_description", uri, fixed = TRUE)) {
      return(description_answer)
    }
    return(enrichment_answer)
  }
  original_post_form_smart <- get("postFormSmart", envir = asNamespace("STRINGdb"))
  assignInNamespace("postFormSmart", answer_for, ns = "STRINGdb")
  on.exit(assignInNamespace("postFormSmart", original_post_form_smart, ns = "STRINGdb"), add = TRUE)

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  proteins <- c("9606.ENSP1", "9606.ENSP2")

  enrichment <- string_db$get_enrichment(proteins)
  checkEquals("Language #Bulgarian", enrichment$description)
  checkEquals(0.01, enrichment$fdr)

  annotations <- string_db$get_annotations(proteins)
  checkEquals("Language #Bulgarian", annotations$description)

  descriptions <- string_db$get_geneset_description(proteins)
  checkEquals("DNA repair #1", descriptions$primary_description)
  checkEquals("Apoptosis", descriptions$tertiary_description)

}
