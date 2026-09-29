\name{get_clusters}
\alias{\S4method{get_clusters}{STRINGdb}}
\title{
get_clusters
}
\description{
Returns a list of clusters of interacting proteins.
Clustering is not available for the directed \code{network_type="regulatory"} network.
See the iGraph (http://igraph.sourceforge.net/) documentation for additional information on the algorithms.
}
\usage{
\S4method{get_clusters}{STRINGdb}(string_ids, algorithm="fastgreedy")
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers.
}
  \item{algorithm}{
algorithm to use for the clustering. You can choose between "fastgreedy", "walktrap", "spinglass" and "edge.betweenness").
}
}
\value{
list of clusters of interacting proteins.
}
\author{
Andrea Franceschini
}
