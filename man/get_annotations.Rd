\name{get_annotations}
\alias{\S4method{get_annotations}{STRINGdb}}
\alias{get_annotations}
\title{
get_annotations
}
\description{
Returns the functional annotation of the given proteins: every term (for example from the Gene Ontology, UniProt keywords, Pfam and InterPro domains, or Reactome pathways) that is assigned to at least one of them.
Unlike \code{get_enrichment}, it does not test which terms are enriched. PubMed publications are not included.
}
\usage{
\S4method{get_annotations}{STRINGdb}(string_ids = NULL)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers (at most 2000).
}
}
\value{
A data frame with one row per term, with columns \code{category}, \code{term_id}, \code{number_of_genes} (the number of input proteins with the term), \code{ratio_in_set} (their share of the input), \code{species}, \code{string_ids} and \code{preferred_names} (those proteins, comma-separated), and \code{description}.
}
\author{
Andrea Franceschini
}
