\name{get_aliases}
\alias{\S4method{get_aliases}{STRINGdb}}
\title{
get_aliases
}
\description{
  Loads and returns STRING aliases. Depending on \code{takeFirst}, this returns either all alias mappings or a single preferred mapping for ambiguous aliases.
}
\usage{
\S4method{get_aliases}{STRINGdb}(takeFirst=TRUE, usePreferredSources=TRUE)
}
\arguments{
  \item{takeFirst}{
boolean indicating whether ambiguous aliases should be collapsed to a single STRING identifier. If \code{FALSE}, all alias mappings are returned. If \code{TRUE}, one mapping is returned for each ambiguous alias.
}
  \item{usePreferredSources}{
boolean indicating whether preferred alias sources should be used to disambiguate aliases when \code{takeFirst=TRUE}. This parameter is ignored when \code{takeFirst=FALSE}.
}
}
\value{
a data frame containing STRING aliases. With \code{takeFirst=FALSE}, all alias mappings are returned. With \code{takeFirst=TRUE}, ambiguous aliases are collapsed to one STRING identifier.
}
\author{
Andrea Franceschini
}
