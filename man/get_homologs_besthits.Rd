\name{get_homologs_besthits}
\alias{\S4method{get_homologs_besthits}{STRINGdb}}
\title{
get_homologs_besthits
}
\description{
Returns the list of closest homologs (as measured by bitscore) of the given input identifiers in all STRING species or single target species.
}
\usage{
\S4method{get_homologs_besthits}{STRINGdb}(string_ids, target_species_id=NULL)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers.
}
  \item{target_species_id}{
 NCBI taxonomy identifier of the species to query for homologs (the species must be present in the STRING database)
}
}
\value{
Data frame containing the best blast hits x species of the given input identifiers.
}
\author{
Andrea Franceschini
}


