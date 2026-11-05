# The test calls STRING; it is deactivated when STRING cannot be reached (helper_live_string.R).
source(system.file("unitTests", "helper_live_string.R", package = "STRINGdb"), local = TRUE)


test_get_enrichment <- function(){

  with_live_string({

    string_db <- STRINGdb$new( version="12.5", species=9606, score_threshold=400 )
    example <- string_db$mp(c('tp53', 'cdk2'));
    enrichment = string_db$get_enrichment(example)
    checkTrue(nrow(enrichment) > 1)

  })

}
