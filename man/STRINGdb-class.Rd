\docType{class}
\name{STRINGdb-class}
\Rdversion{1.1}
\alias{STRINGdb-class}
\alias{STRINGdb}

\title{Class \code{"STRINGdb"}}
\description{
The R package STRINGdb provides a convenient interface to STRING, a protein-protein interaction database and functional enrichment analysis tool, for R/Bioconductor users.
Please look at the manual/vignette for additional information and examples on how to use the package.
STRING is a database of known and predicted protein-protein interactions. It contains information from numerous
sources, including experimental repositories, computational prediction methods, and public text collections. Each interaction is associated with a combined confidence score that integrates the different evidence channels.
STRING v12.0 contains information on 59.3 million proteins from 12,535 organisms and more than 20 billion interactions.
The STRING web interface is freely accessible at: https://string-db.org/
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
Szklarczyk D, Kirsch R, Koutrouli M, Nastou K, Mehryary F, Hachilif R, Gable AL, Fang T, Doncheva NT, Pyysalo S, Bork P, Jensen LJ, von Mering C. The STRING database in 2023: protein-protein association networks and functional enrichment analyses for any sequenced genome of interest. Nucleic Acids Res. 2023 Jan 6;51(D1):D638-D646. doi: 10.1093/nar/gkac1000.
}
\author{
Andrea Franceschini and Damian Szklarczyk
}
\examples{
showClass("STRINGdb")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{annotations}:}{Object of class \code{data.frame} ~~ }
    \item{\code{annotations_description}:}{Object of class \code{data.frame} ~~ }
    \item{\code{graph}:}{Object of class \code{igraph} ~~ }
    \item{\code{proteins}:}{Object of class \code{data.frame} ~~ }
    \item{\code{speciesList}:}{Object of class \code{data.frame} ~~ }
    \item{\code{species}:}{Object of class \code{numeric} ~~ }
    \item{\code{version}:}{Object of class \code{character} ~~ }
    \item{\code{input_directory}:}{Object of class \code{character} ~~ }
    \item{\code{backgroundV}:}{Object of class \code{vector} ~~ }
    \item{\code{score_threshold}:}{Object of class \code{numeric} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{set_background(background_vector)}:}{ ~~ }
    \item{\code{post_payload(stringIds, colors, comments, links, iframe_urls, logo_imgF, legend_imgF)}:}{ ~~ }
    \item{\code{plot_network(string_ids, payload_id, required_score)}:}{ ~~ }
    \item{\code{plot_ppi_enrichment(string_ids, file, sliceWindow, edgeWindow, windowExtendedReferenceThreshold, minVal, title)}:}{ ~~ }
    \item{\code{map(my_data_frame, my_data_frame_id_col_names, takeFirst, removeUnmappedRows, quiet)}:}{ ~~ }
    \item{\code{load()}:}{ ~~ }
    \item{\code{get_term_proteins(term_ids, string_ids, enableIEA)}:}{ ~~ }
    \item{\code{get_summary(string_ids)}:}{ ~~ }
    \item{\code{get_subnetwork(string_ids)}:}{ ~~ }
    \item{\code{get_ppi_enrichment_full(string_ids, sliceWindow, edgeWindow, windowExtendedReferenceThreshold, growingWindowLimit)}:}{ ~~ }
    \item{\code{get_ppi_enrichment(string_ids)}:}{ ~~ }
    \item{\code{get_proteins()}:}{ ~~ }
    \item{\code{get_enrichment_figure(string_ids, category="Process", file, output_format="image", group_by_similarity, color_palette="mint_blue", number_of_term_shown, x_axis="signal", caller_identity="STRINGdb-package")}:}{ ~~ }
    \item{\code{get_png(string_ids, required_score, network_flavor, file, payload_id, output_format, network_term_id, hide_node_labels, hide_disconnected_nodes, block_structure_pics_in_bubbles, flat_node_design=TRUE, center_node_labels, custom_label_font_size, caller_identity)}:}{ ~~ }
    \item{\code{get_neighbors(string_ids)}:}{ ~~ }
    \item{\code{get_interaction_partners(string_ids, required_score, limit)}:}{ ~~ }
    \item{\code{get_link(string_ids, required_score, network_flavor, payload_id, network_term_id, hide_node_labels, hide_disconnected_nodes, block_structure_pics_in_bubbles, flat_node_design=TRUE, center_node_labels, custom_label_font_size, caller_identity)}:}{ ~~ }
    \item{\code{get_interactions(string_ids)}:}{ ~~ }
    \item{\code{get_homologs_besthits(string_ids, symbets, target_species_id, bitscore_threshold)}:}{ ~~ }
    \item{\code{get_homologs(string_ids, target_species_id, bitscore_threshold)}:}{ ~~ }
    \item{\code{get_graph()}:}{ ~~ }
    \item{\code{get_enrichment(string_ids, category, methodMT, iea)}:}{ ~~ }
    \item{\code{get_clusters(string_ids, algorithm)}:}{ ~~ }
    \item{\code{get_annotations_desc()}:}{ ~~ }
    \item{\code{get_annotations()}:}{ ~~ }
    \item{\code{load_all()}:}{ ~~ }
    \item{\code{initialize(...)}:}{ ~~ }
    \item{\code{add_proteins_description(screen)}:}{ ~~ }
    \item{\code{add_diff_exp_color(screen, logFcColStr)}:}{ ~~ }
    \item{\code{show()}:}{ ~~ }
  }
}
