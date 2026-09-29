\name{get_enrichment_figure}
\alias{\S4method{get_enrichment_figure}{STRINGdb}}
\title{
get_enrichment_figure
}
\description{
Returns a STRING enrichment figure for the given identifiers and enrichment category.
}
\usage{
\S4method{get_enrichment_figure}{STRINGdb}(string_ids, category = "Process", file = NULL, output_format = "image", group_by_similarity = NULL, color_palette = "mint_blue", number_of_term_shown = NULL, x_axis = "signal", caller_identity = "STRINGdb-package")
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
maximum number of terms displayed on the plot.
}
  \item{x_axis}{
variable used on the x-axis and for ranking the terms: \code{"signal"}, \code{"strength"}, \code{"FDR"}, or \code{"gene_count"}.
}
  \item{caller_identity}{
caller identifier sent to STRING.
}
}
\value{
For \code{output_format="image"} and \code{output_format="highres_image"}, returns a PNG image array. For \code{output_format="svg"}, returns the SVG markup as text.
}
\author{
Damian Szklarczyk
}
