\name{get_paralogs}
\alias{\S4method{get_paralogs}{STRINGdb}}
\title{
get_paralogs
}
\description{
Returns the list of paralogs of the given input in their species.
}
\usage{
\S4method{get_paralogs}{STRINGdb}(string_ids)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers.
}
}
\value{
Data frame containing the within-species homology hits of the input identifiers, with columns \code{ncbiTaxonId_A}, \code{stringId_A}, \code{ncbiTaxonId_B}, \code{stringId_B}, and \code{bitscore}.
}
\author{
Andrea Franceschini
}

