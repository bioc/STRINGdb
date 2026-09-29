test_get_annotations <- function(){

  string_db <- STRINGdb$new( version="12.5", species=9606, score_threshold=400 )
  example <- string_db$mp(c('tp53', 'cdk2'));
  enrichment = string_db$get_annotations(example)
  checkTrue(nrow(enrichment) > 1)

}
