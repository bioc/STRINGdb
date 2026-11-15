\name{get_ppi_enrichment}
\alias{\S4method{get_ppi_enrichment}{STRINGdb}}
\alias{get_ppi_enrichment}
\title{
get_ppi_enrichment
}
\description{
Returns a pvalue representing the enrichment in interactions of the list of proteins 
          (i.e. the probability to obtain such a number of interactions by chance).
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

