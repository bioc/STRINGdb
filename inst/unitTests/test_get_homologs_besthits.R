# The test calls STRING; it is deactivated when STRING cannot be reached (helper_live_string.R).
source(system.file("unitTests", "helper_live_string.R", package = "STRINGdb"), local = TRUE)


test_get_homologs_besthits <- function() {

  with_live_string({

    string_db <- STRINGdb$new( version="12.5", species=9606, score_threshold=400 )
    stringid = string_db$mp('CDK1')
    homologs = string_db$get_homologs_besthits( stringid )
    checkTrue(nrow(homologs) > 0)

  })

}
