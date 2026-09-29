\name{get_geneset_description}
\alias{\S4method{get_geneset_description}{STRINGdb}}
\title{
get_geneset_description
}
\description{
Returns concise primary, secondary, and tertiary descriptions of a protein set using the STRING gene-set description API.
}
\usage{
\S4method{get_geneset_description}{STRINGdb}(string_ids, caller_identity="STRINGdb-package")
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers.
}
  \item{caller_identity}{
caller identifier sent to STRING.
}
}
\value{
A one-row data frame containing primary, secondary, and tertiary descriptions when they are available.
}
\author{
Damian Szklarczyk
}
