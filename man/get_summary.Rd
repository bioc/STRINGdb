\name{get_summary}
\alias{\S4method{get_summary}{STRINGdb}}
\title{
get_summary
}
\description{
Returns a summary of the STRING sub-network containing the identifiers provided in input.
The summary and its interaction-enrichment p-value use the object's selected network type.
}
\usage{
\S4method{get_summary}{STRINGdb}(string_ids, required_score=NULL)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers
}
  \item{required_score}{
minimum STRING combined score of the interactions (default \code{NULL}: the \code{score_threshold} of the object)
}
}
\value{
Returns a summary (i.e. a text description) of the STRING sub-network containing the identifiers provided in input.
}
\author{
Andrea Franceschini
}

