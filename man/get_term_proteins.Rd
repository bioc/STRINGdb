\name{get_term_proteins}
\alias{\S4method{get_term_proteins}{STRINGdb}}
\alias{get_term_proteins}
\title{
get_term_proteins
}
\description{
Deprecated since 2020: this method returns no proteins, only a deprecation warning.
To get the terms of given proteins, use \code{\link{get_annotations}}.
}
\usage{
\S4method{get_term_proteins}{STRINGdb}(term_ids, string_ids=NULL, enableIEA=TRUE)
}
\arguments{
  \item{term_ids}{
vector of terms (not used).
}
  \item{string_ids}{
a vector of STRING identifiers (not used).
}
  \item{enableIEA}{
not used.
}
}
\value{
The deprecation message, invisibly.
}
\author{
Andrea Franceschini
}
