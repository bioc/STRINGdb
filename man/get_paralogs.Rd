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
Data frame containing the best blast hits x species of the given input identifiers.
}
\author{
Andrea Franceschini
}


