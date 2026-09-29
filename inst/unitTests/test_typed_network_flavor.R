test_typed_network_flavor <- function() {

  checkTrue(identical(STRINGdb:::normalize_network_flavor("typed", "full"), "typed"))
  checkTrue(identical(suppressWarnings(STRINGdb:::normalize_network_flavor("actions", "functional")), "typed"))
  checkTrue(identical(STRINGdb:::normalize_network_flavor("evidence", "regulatory"), "evidence"))
  typed_controls <- STRINGdb:::normalize_typed_network_controls(
    TRUE, FALSE, FALSE, "typed"
  )
  checkTrue(identical(typed_controls$typed_physical_edges, 1L))
  checkTrue(identical(typed_controls$typed_regulatory_edges, 0L))

  typed_regulatory_error <- tryCatch(
    {
      STRINGdb:::normalize_network_flavor("typed", "regulatory")
      FALSE
    },
    error = function(e) TRUE
  )
  checkTrue(typed_regulatory_error)

  typed_controls_error <- tryCatch(
    {
      STRINGdb:::normalize_typed_network_controls(TRUE, NULL, NULL, "evidence")
      FALSE
    },
    error = function(e) TRUE
  )
  checkTrue(typed_controls_error)
}
