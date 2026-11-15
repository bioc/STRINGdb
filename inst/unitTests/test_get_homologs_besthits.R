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


test_get_homologs_besthits_several_species <- function() {

  # Several target species are sent in one species_b parameter, separated by a carriage return; STRING
  # does not split the text "%0d" in a form request and would use only the first species. The request
  # is captured instead of sent: postFormSmart is replaced for this test, and restored at its end.
  sent_params <- list()
  capture_request <- function(uri, .params = list(), .opts = list(), .ctype = "text", .stop_on_client_error = FALSE) {
    sent_params[[length(sent_params) + 1]] <<- .params
    return("stringId_A\tncbiTaxonId_A\tstringId_B\tncbiTaxonId_B\tbitscore")
  }
  original_post_form_smart <- get("postFormSmart", envir = asNamespace("STRINGdb"))
  assignInNamespace("postFormSmart", capture_request, ns = "STRINGdb")
  on.exit(assignInNamespace("postFormSmart", original_post_form_smart, ns = "STRINGdb"), add = TRUE)

  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)
  string_db$get_homologs_besthits("9606.ENSP00000269305", target_species_id = c(10090, 7227))
  string_db$get_homologs_besthits("9606.ENSP00000269305", target_species_id = 10090)
  string_db$get_homologs_besthits("9606.ENSP00000269305")
  # NA species are left out (STRING would ignore the whole list); none left means all species
  string_db$get_homologs_besthits("9606.ENSP00000269305", target_species_id = c(10090, NA))
  string_db$get_homologs_besthits("9606.ENSP00000269305", target_species_id = NA)

  checkEquals("10090\r7227", sent_params[[1]]$species_b)
  checkEquals("10090", sent_params[[2]]$species_b)
  checkTrue(is.null(sent_params[[3]]$species_b))
  checkEquals("10090", sent_params[[4]]$species_b)
  checkTrue(is.null(sent_params[[5]]$species_b))

}
