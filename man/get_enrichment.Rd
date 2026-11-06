\name{get_enrichment}
\alias{\S4method{get_enrichment}{STRINGdb}}
\alias{get_enrichment}
\title{
get_enrichment
}
\description{
Returns the enrichment in pathways of the vector of STRING proteins that is given in input.
}
\usage{
\S4method{get_enrichment}{STRINGdb}(string_ids, category = "All", methodMT = NULL, iea = NULL, minScore = NULL, max_fdr = NULL, min_gene_count = NULL, min_strength = NULL, min_signal = NULL, merge_similarity = NULL, add_scores = FALSE)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers.
}
  \item{category}{
category for which to return the enrichment (i.e. "Process", "Component", "Function", "KEGG", "Pfam", "InterPro"). The default \code{"All"} returns all categories.
}
  \item{methodMT}{
deprecated: only the FDR correction is available. A value is ignored, with a warning.
}
  \item{iea}{
deprecated: a value is ignored, with a warning.
}
  \item{minScore}{
deprecated: a value is ignored, with a warning.
}
  \item{max_fdr}{
optional filter: the highest FDR of a term, above \code{0} and at most \code{1}.
}
  \item{min_gene_count}{
optional filter: the lowest number of input proteins annotated with a term, a whole number of at least \code{1}.
}
  \item{min_strength}{
optional filter: the lowest strength of a term, \code{log10(observed / expected)}.
}
  \item{min_signal}{
optional filter: the lowest signal of a term, STRING's balance of strength and FDR.
}
  \item{merge_similarity}{
optional: merges the terms whose protein sets overlap at least this much (above \code{0}, at most \code{1}; \code{1} merges identical sets only) into the most significant of them.
}
  \item{add_scores}{
\code{TRUE} adds STRING's \code{strength} and \code{signal} columns to the returned table, before \code{p_value}. The default \code{FALSE} returns the table without them.
}
}
\details{
STRING applies the optional filters like the filters of the enrichment table on its website. Without them (the default \code{NULL}), no term is filtered.

By default the returned table has no strength and signal columns. With \code{add_scores=TRUE}, STRING inserts them before \code{p_value}, so that the columns are \code{category}, \code{term}, \code{number_of_genes}, \code{number_of_genes_in_background}, \code{ncbiTaxonId}, \code{inputGenes}, \code{preferredNames}, \code{strength}, \code{signal}, \code{p_value}, \code{fdr} and \code{description}. The strength of a term is \code{log10(observed / expected)}; its signal is STRING's balance of strength and FDR.

The filters and \code{add_scores} need STRING version 12.5 or later. An older version, such as \code{version="12.0"}, would ignore them, so they are not used there, with a warning.
}
\value{
Data frame containing the enrichment in pathways of the vector of STRING proteins that is given in input.
Its columns \code{inputGenes} and \code{preferredNames} list the term's proteins as comma-separated text; in the next Bioconductor release they will become list columns (a character vector for each term), here and in \code{get_values_ranks_enrichment}.
}
\author{
Andrea Franceschini
}


