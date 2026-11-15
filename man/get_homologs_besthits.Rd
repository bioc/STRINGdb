\name{get_homologs_besthits}
\alias{\S4method{get_homologs_besthits}{STRINGdb}}
\alias{get_homologs_besthits}
\title{
get_homologs_besthits
}
\description{
Returns the list of closest homologs (as measured by bitscore) of the given input identifiers in all STRING species, or in the species given by \code{target_species_id}.
}
\usage{
\S4method{get_homologs_besthits}{STRINGdb}(string_ids, target_species_id=NULL)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers.
}
  \item{target_species_id}{
NCBI taxonomy identifiers of one or several species to query for homologs, e.g. \code{c(10090, 7227)} (the species must be present in the STRING database). By default, all species.
}
}
\value{
Data frame containing the best blast hits x species of the given input identifiers.
}
\author{
Andrea Franceschini
}


