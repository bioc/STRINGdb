\name{get_geneset_description}
\alias{\S4method{get_geneset_description}{STRINGdb}}
\title{
get_geneset_description
}
\description{
Returns concise primary, secondary, and tertiary descriptions of a protein set using the STRING gene-set description API.
}
\usage{
\S4method{get_geneset_description}{STRINGdb}(string_ids, caller_identity=NULL)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers.
}
  \item{caller_identity}{
ignored, with a warning (once per R session): the package sends its own caller identity (\code{"STRINGdb-package"}) with every request to STRING. It is kept, with the default \code{NULL}, so that code written for earlier versions still runs, and will be removed in the next Bioconductor release, so do not specify it.
}
}
\value{
A one-row data frame containing primary, secondary, and tertiary descriptions when they are available.
}
\author{
Damian Szklarczyk
}
