\name{get_interactions}
\alias{\S4method{get_interactions}{STRINGdb}}
\title{
get_interactions
}
\description{
Shows the interactions in between the proteins that are given in input.
}
\usage{
\S4method{get_interactions}{STRINGdb}(string_ids)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers
}
}
\value{
Data frame containing the interactions in between the input proteins. The leading columns are \code{from}, \code{to}, \code{combined_score}, \code{from_name} and \code{to_name}, followed by any additional loaded edge attributes.
}
\author{
Damian Szklarczyk
}
