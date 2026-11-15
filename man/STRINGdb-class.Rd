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
STRING v12.5 contains information on 59.3 million proteins from 12,535 organisms and more than 28 billion interactions. It provides functional, physical, and directed regulatory networks.
The STRING web interface is freely accessible at: https://string-db.org/
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
Szklarczyk D, Nastou K, Koutrouli M, Kirsch R, Mehryary F, Hachilif R, Hu D, Peluso ME, Huang Q, Fang T, Doncheva NT, Pyysalo S, Bork P, Jensen LJ, von Mering C. The STRING database in 2025: protein networks with directionality of regulation. Nucleic Acids Res. 2025 Jan 6;53(D1):D730-D737. doi: 10.1093/nar/gkae1113.
}
\author{
Andrea Franceschini and Damian Szklarczyk
}
\examples{
showClass("STRINGdb")
}
\keyword{classes}
\section{Fields}{
  The settings are given to \code{STRINGdb$new()}, e.g. \code{STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)}:
  \describe{
    \item{\code{species}:}{The NCBI taxonomy identifier of the organism, e.g. \code{9606} for human or \code{10090} for mouse. Without it, 9606 is used and a warning is printed.}
    \item{\code{version}:}{The STRING version, e.g. \code{"12.5"}; a version without a dot gets \code{".0"} (\code{"11"} means \code{"11.0"}). For the versions known to the package (11.0, 11.0b, 11.5, 12.0 and 12.5) the object is created without a request to STRING, so it can also be created offline. Without a version, STRING is asked for its current version; when STRING cannot be reached within a few seconds, the newest known version is used and a warning is printed. Any other version is looked up on STRING, and creating the object stops with an error when that version is not available or STRING cannot be reached.}
    \item{\code{score_threshold}:}{The minimum combined score of the interactions in the local network (default 400). Scores are on a scale from 0 to 1000: STRING's website shows 400 as 0.4. A value below 1 is set to 1.}
    \item{\code{network_type}:}{One of \code{"full"} (the functional network, the default), \code{"physical"}, or \code{"regulatory"}. The regulatory network requires STRING v12.5 or later.}
    \item{\code{link_data}:}{One of \code{"combined_only"} (the default: only the combined score), \code{"detailed"} (also the scores of the evidence channels), or \code{"full"} (also the transferred scores).}
    \item{\code{input_directory}:}{The folder where the downloaded STRING files are kept and reused. By default, or with \code{""}, a temporary folder that is deleted when the R session ends.}
    \item{\code{backgroundV}:}{The background: a vector of STRING identifiers, set with \code{set_background}.}
    \item{\code{protocol}:}{\code{"https"} (the default) or \code{"http"}, for the download files only. Calls to STRING's API always use https.}
  }
  The other fields hold the data that the methods download and load: \code{proteins}, \code{graph}, \code{homology_graph} and the \code{aliases_} tables. \code{stable_url} and \code{file_version} follow from the version. \code{annotations}, \code{annotations_description} and \code{pathways_benchmark_blackList} are no longer used.
}
\section{Methods}{
  Each method has its own help page, e.g. \code{?get_interactions}. The methods that have been deprecated since 2020 do nothing and are not listed.
  \describe{
    \item{\code{map(my_data_frame, my_data_frame_id_col_names, takeFirst, removeUnmappedRows, quiet, usePreferredSources)}:}{ adds a column of STRING identifiers to a data frame of gene or protein names (\link{map}). }
    \item{\code{mp(protein_aliases)}:}{ maps a vector of gene or protein names to STRING identifiers (\link{mp}). }
    \item{\code{get_aliases(takeFirst, usePreferredSources)}:}{ the names (aliases) of the species' STRING proteins (\link{get_aliases}). }
    \item{\code{get_proteins()}:}{ the species' STRING proteins, with their preferred names, sizes and descriptions (\link{get_proteins}). }
    \item{\code{add_proteins_description(screen)}:}{ adds the proteins' preferred names and descriptions to a data frame (\link{add_proteins_description}). }
    \item{\code{get_interactions(string_ids)}:}{ the interactions among the given proteins, as a data frame (\link{get_interactions}). }
    \item{\code{get_interaction_partners(string_ids, required_score, limit)}:}{ the interaction partners of the given proteins (\link{get_interaction_partners}). }
    \item{\code{get_neighbors(string_ids, mode="all")}:}{ the neighbours of the given proteins in the network (\link{get_neighbors}). }
    \item{\code{get_subnetwork(string_ids)}:}{ the network among the given proteins, as an igraph object (\link{get_subnetwork}). }
    \item{\code{get_graph()}:}{ the whole network of the species, as an igraph object (\link{get_graph}). }
    \item{\code{get_bioc_graph()}:}{ the whole network as a graphNEL object of the graph package (\link{get_bioc_graph}). }
    \item{\code{load()}:}{ downloads and loads the network of the species into the object. }
    \item{\code{load_all()}:}{ downloads and loads the proteins, their names and the network, e.g. before saving the object (\link{load_all}). }
    \item{\code{set_background(background_vector)}:}{ sets the background for the enrichment and the network (\link{set_background}). }
    \item{\code{get_clusters(string_ids, algorithm)}:}{ clusters of the network of the given proteins (\link{get_clusters}). }
    \item{\code{plot_network(string_ids, payload_id, required_score, add_link, network_flavor, add_summary, typed_physical_edges, typed_regulatory_edges, show_regulatory_signs, add_color_nodes, add_white_nodes, channels)}:}{ draws STRING's picture of the network of the given proteins (\link{plot_network}). }
    \item{\code{get_png(string_ids, required_score, network_flavor, file, payload_id, output_format, network_term_id, hide_node_labels, hide_disconnected_nodes, block_structure_pics_in_bubbles, flat_node_design=TRUE, center_node_labels, custom_label_font_size, caller_identity, typed_physical_edges, typed_regulatory_edges, show_regulatory_signs, network_clustering_algorithm, network_clustering_parameter, cluster_aware_layout, network_clustering_edge_visibility, show_cluster_names, cluster_name_size, add_color_nodes, add_white_nodes, channels)}:}{ STRING's picture of the network, as an image or SVG text, optionally saved to a file (\link{get_png}). }
    \item{\code{get_link(string_ids, required_score, network_flavor, payload_id, network_term_id, hide_node_labels, hide_disconnected_nodes, block_structure_pics_in_bubbles, flat_node_design=TRUE, center_node_labels, custom_label_font_size, caller_identity, typed_physical_edges, typed_regulatory_edges, show_regulatory_signs, add_color_nodes, add_white_nodes, channels)}:}{ a link to the network on STRING's website (\link{get_link}). }
    \item{\code{post_payload(stringIds, colors, comments, links, iframe_urls, logo_imgF, legend_imgF)}:}{ uploads colours and other data to show on STRING's pictures and pages, and returns a payload identifier (\link{post_payload}). }
    \item{\code{add_diff_exp_color(screen, logFcColStr)}:}{ adds a colour column for the fold changes of a data frame, for \code{post_payload} (\link{add_diff_exp_color}). }
    \item{\code{get_summary(string_ids, required_score)}:}{ a text summary of the network of the given proteins (\link{get_summary}). }
    \item{\code{get_ppi_enrichment(string_ids)}:}{ tests whether the given proteins have more interactions among themselves than expected (\link{get_ppi_enrichment}). }
    \item{\code{get_enrichment(string_ids, category, methodMT, iea, minScore, max_fdr, min_gene_count, min_strength, min_signal, merge_similarity, add_scores=FALSE)}:}{ the functional enrichment of the given proteins (\link{get_enrichment}). }
    \item{\code{get_enrichment_figure(string_ids, category="Process", file, output_format="image", group_by_similarity, color_palette="mint_blue", number_of_term_shown, x_axis="signal", caller_identity=NULL, graph_type="dotplot", sort_by, max_fdr, min_gene_count, min_strength, min_signal, merge_similarity)}:}{ STRING's figure of the enrichment of the given proteins (\link{get_enrichment_figure}). }
    \item{\code{get_annotations(string_ids)}:}{ all the terms annotated to the given proteins (\link{get_annotations}). }
    \item{\code{get_geneset_description(string_ids, caller_identity=NULL)}:}{ short descriptions of the protein set, based on its enrichment (\link{get_geneset_description}). }
    \item{\code{get_values_ranks_enrichment(string_ids, values, max_fdr=0.05, rank_direction="descending", timeout=7200, quiet=FALSE)}:}{ a GSEA-like enrichment analysis of a whole ranked experiment, computed on STRING's servers (not locally): STRING's values/ranks enrichment, run on STRING's job queue; with a table of values, one job for each experiment, run side by side (\link{get_values_ranks_enrichment}). }
    \item{\code{submit_values_ranks_enrichment(string_ids, values, max_fdr=0.05, rank_direction="descending", quiet=FALSE)}:}{ submits the values/ranks enrichment (GSEA-like, computed on STRING's servers) and returns the job identifiers at once, without blocking the R session (\link{submit_values_ranks_enrichment}). }
    \item{\code{get_values_ranks_status(job_ids=NULL)}:}{ the state of values/ranks jobs, from one request (\link{get_values_ranks_status}). }
    \item{\code{get_values_ranks_results(job_ids, timeout=7200, quiet=FALSE)}:}{ the results of values/ranks jobs (GSEA-like, computed on STRING's servers), waiting for the unfinished ones (\link{get_values_ranks_results}). }
    \item{\code{get_api_key()}:}{ the STRING API key of this computer for the values/ranks enrichment, GSEA-like and computed on STRING's servers (\link{get_api_key}). }
    \item{\code{get_homologs_besthits(string_ids, target_species_id)}:}{ the closest homologs of the given proteins in other species (\link{get_homologs_besthits}). }
    \item{\code{get_paralogs(string_ids)}:}{ the sequence similarity between the given proteins (\link{get_paralogs}). }
    \item{\code{remove_homologous_interactions(interactions_dataframe, bitscore_threshold)}:}{ removes the interactions between similar proteins (\link{remove_homologous_interactions}). }
    \item{\code{show()}:}{ prints the object. }
    \item{\code{initialize(...)}:}{ creates the object; called by \code{STRINGdb$new()}. }
  }
}
