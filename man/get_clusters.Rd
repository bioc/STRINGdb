\name{get_clusters}
\alias{\S4method{get_clusters}{STRINGdb}}
\alias{get_clusters}
\title{
get_clusters
}
\description{
Returns a list of clusters of interacting proteins.
Clustering is not available for the directed \code{network_type="regulatory"} network.
See the igraph documentation (\url{https://igraph.org}) for additional information on the algorithms.
}
\usage{
\S4method{get_clusters}{STRINGdb}(string_ids, algorithm="fastgreedy")
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers.
}
  \item{algorithm}{
algorithm to use for the clustering: "fastgreedy" (default), "walktrap" or "edge.betweenness". Any other value stops with an error.
}
}
\value{
list of clusters of interacting proteins.
}
\author{
Andrea Franceschini
}
