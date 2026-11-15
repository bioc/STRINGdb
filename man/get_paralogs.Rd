\name{get_paralogs}
\alias{\S4method{get_paralogs}{STRINGdb}}
\alias{get_paralogs}
\title{
get_paralogs
}
\description{
Returns the sequence similarity (Smith-Waterman bitscore) between the given proteins, including each protein with itself.
It compares only the given proteins: it does not search their proteome for paralogs. A single protein therefore returns only its similarity with itself.
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
Data frame with one row for each protein with itself and two rows (A-B and B-A) for each pair of similar input proteins, with columns \code{ncbiTaxonId_A}, \code{stringId_A}, \code{ncbiTaxonId_B}, \code{stringId_B}, and \code{bitscore}. STRING reports no similarity with a bitscore below 50.
}
\author{
Andrea Franceschini
}

