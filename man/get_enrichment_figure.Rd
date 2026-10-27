\name{get_enrichment_figure}
\alias{\S4method{get_enrichment_figure}{STRINGdb}}
\title{
get_enrichment_figure
}
\description{
Returns a STRING enrichment figure for the given identifiers and enrichment category.
}
\usage{
\S4method{get_enrichment_figure}{STRINGdb}(string_ids, category = "Process", file = NULL, output_format = "image", group_by_similarity = NULL, color_palette = "mint_blue", number_of_term_shown = NULL, x_axis = "signal", caller_identity = "STRINGdb-package", graph_type = "dotplot", sort_by = NULL, max_fdr = NULL, min_gene_count = NULL, min_strength = NULL, min_signal = NULL, merge_similarity = NULL)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers.
}
  \item{category}{
enrichment category to visualize. Supported categories are \code{"Process"}, \code{"Function"}, \code{"Component"}, \code{"Keyword"}, \code{"KEGG"}, \code{"RCTM"}, \code{"HPO"}, \code{"MPO"}, \code{"DPO"}, \code{"WPO"}, \code{"ZPO"}, \code{"FYPO"}, \code{"Pfam"}, \code{"SMART"}, \code{"InterPro"}, \code{"PMID"}, \code{"NetworkNeighborAL"}, \code{"COMPARTMENTS"}, \code{"TISSUES"}, \code{"DISEASES"}, and \code{"WikiPathways"}.
}
  \item{file}{
file where to save the image output.
}
  \item{output_format}{
STRING image output format: \code{"image"}, \code{"highres_image"} or \code{"svg"}.
}
  \item{group_by_similarity}{
threshold for visually grouping related terms on the plot, from \code{0.1} to \code{1}.
}
  \item{color_palette}{
color palette used to represent FDR values. Supported values are \code{"mint_blue"}, \code{"lime_emerald"}, \code{"green_blue"}, \code{"peach_purple"}, \code{"straw_navy"}, and \code{"yellow_pink"}.
}
  \item{number_of_term_shown}{
maximum number of terms displayed on the plot. STRING shows at most 100.
}
  \item{x_axis}{
variable used on the x-axis and, without \code{sort_by}, for ranking the terms: \code{"signal"}, \code{"strength"}, \code{"FDR"}, or \code{"gene_count"}.
}
  \item{caller_identity}{
caller identifier sent to STRING.
}
  \item{graph_type}{
the kind of plot: \code{"dotplot"} (default) or \code{"barplot"}.
}
  \item{sort_by}{
variable that decides which terms are shown and in which order: \code{"signal"}, \code{"strength"}, \code{"FDR"}, or \code{"gene_count"}. The default \code{NULL} uses the \code{x_axis} variable.
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
optional: merges the terms whose protein sets overlap at least this much (above \code{0}, at most \code{1}; \code{1} merges identical sets only) into the most significant of them. Unlike \code{group_by_similarity}, which only groups related terms on the plot, merged terms are not shown.
}
}
\details{
STRING applies the optional filters like the filters of the enrichment table on its website; without them (the default \code{NULL}), no term is filtered. When STRING refuses the request, the method stops with STRING's explanation.

\code{graph_type}, \code{sort_by} and the filters need STRING version 12.5 or later. An older version, such as \code{version="12.0"}, would ignore them, so they are not used there, with a warning.
}
\value{
For \code{output_format="image"} and \code{output_format="highres_image"}, returns a PNG image array. For \code{output_format="svg"}, returns the SVG markup as text.
}
\author{
Damian Szklarczyk
}
