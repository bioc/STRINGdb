\name{get_interaction_partners}
\alias{\S4method{get_interaction_partners}{STRINGdb}}
\title{
get_interaction_partners
}
\description{
Returns the interaction partners of the input proteins using the locally loaded STRING graph.
The returned data frame preserves the edge attributes available for the current \code{link_data} setting.
}
\usage{
\S4method{get_interaction_partners}{STRINGdb}(string_ids, required_score=NULL, limit=NULL)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers
}
  \item{required_score}{
optional minimum combined score for returned partners. This value cannot be below the \code{score_threshold} used to load the local graph.
}
  \item{limit}{
optional maximum number of partner rows returned per query protein
}
}
\value{
Data frame containing one row per query-partner interaction. The leading columns are \code{from}, \code{to}, \code{combined_score}, \code{from_name} and \code{to_name}, followed by any additional edge attributes loaded for the current \code{link_data} mode. Here \code{from} is the queried protein and \code{to} is its interaction partner. If \code{limit} is used, rows are ordered by \code{combined_score}.
}
\author{
Damian Szklarczyk
}
