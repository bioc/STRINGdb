\name{get_ppi_enrichment}
\alias{\S4method{get_ppi_enrichment}{STRINGdb}}
\alias{get_ppi_enrichment}
\title{
get_ppi_enrichment
}
\description{
Tests whether the given proteins have more interactions among themselves than expected by chance.
Returns the p-value, with the observed and the expected numbers of interactions.
The calculation uses the object's selected functional, physical, or regulatory network type.
}
\usage{
\S4method{get_ppi_enrichment}{STRINGdb}(string_ids)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers
}
}
\value{
A list with three elements: \code{enrichment}, the p-value of the enrichment in interactions (the probability to obtain at least as many interactions by chance); \code{edges}, the number of interactions among the proteins; and \code{lambda}, the expected number of interactions.
}
\author{
Andrea Franceschini
}

