\name{get_png}
\alias{\S4method{get_png}{STRINGdb}}
\title{
get_png
}
\description{
Returns a STRING network image for the given identifiers or for a STRING functional term.
}
\usage{
\S4method{get_png}{STRINGdb}(string_ids=NULL, required_score=NULL, network_flavor="evidence", file=NULL, payload_id=NULL, output_format="image", network_term_id=NULL, hide_node_labels=NULL, hide_disconnected_nodes=NULL, block_structure_pics_in_bubbles=NULL, flat_node_design=TRUE, center_node_labels=NULL, custom_label_font_size=NULL, caller_identity="STRINGdb-package", typed_physical_edges=NULL, typed_regulatory_edges=NULL, show_regulatory_signs=NULL, network_clustering_algorithm=NULL, network_clustering_parameter=NULL, cluster_aware_layout=NULL, network_clustering_edge_visibility=NULL, show_cluster_names=NULL, cluster_name_size=NULL)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers. Can be omitted when \code{network_term_id} is provided.
}
  \item{required_score}{
minimum STRING combined score of the interactions 
(if left NULL we get the combined score of the object, which is 400 by default).
}
  \item{network_flavor}{
specify the flavor of the network: \code{"evidence"}, \code{"confidence"}, or \code{"typed"}. The typed view is available only when \code{network_type="full"} (or its \code{"functional"} alias). \code{"actions"} is accepted as a compatibility alias for \code{"typed"}.
}
  \item{file}{
file where to save the image output.
}
  \item{payload_id}{
identifier of the payload.
}
  \item{output_format}{
STRING image output format: \code{"image"}, \code{"highres_image"} or \code{"svg"}.
}
  \item{network_term_id}{
functional term identifier used by STRING instead of explicit protein identifiers.
}
  \item{hide_node_labels}{
hides all protein names from the picture. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}.
}
  \item{hide_disconnected_nodes}{
hides proteins that are not connected to any other protein in the network. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}.
}
  \item{block_structure_pics_in_bubbles}{
disables structure pictures inside the bubbles. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}.
}
  \item{flat_node_design}{
disables 3D bubble design. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}. Default is \code{TRUE}.
}
  \item{center_node_labels}{
centers protein names on nodes. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}.
}
  \item{custom_label_font_size}{
changes the font size of the protein names (from 5 to 50).
}
  \item{caller_identity}{
caller identifier sent to STRING.
}
  \item{typed_physical_edges}{
include physical edges in the typed network view. Available only with \code{network_flavor="typed"}. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}.
}
  \item{typed_regulatory_edges}{
include regulatory edges in the typed network view. Available only with \code{network_flavor="typed"}. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}.
}
  \item{show_regulatory_signs}{
show positive/negative regulatory signs in the typed network view. Requires \code{typed_regulatory_edges=TRUE}. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}.
}
  \item{network_clustering_algorithm}{
clusters the network and colours the proteins by cluster, as the Clusters tab of the STRING website: \code{"kmeans"}, \code{"mcl"} or \code{"leiden"} (in any case). The default \code{NULL} draws the network without clusters; the other clustering options need an algorithm.
}
  \item{network_clustering_parameter}{
the parameter of the clustering algorithm: for k-means the number of clusters (a whole number of at least 1, default \code{3}), for MCL the inflation (from \code{1.1} to \code{10}, default \code{3}), and for Leiden the resolution (from \code{0.1} to \code{10}, default \code{1}).
}
  \item{cluster_aware_layout}{
lays the network out by its clusters. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}.
}
  \item{network_clustering_edge_visibility}{
how the edges between clusters are drawn: \code{"faded"} (STRING's default), \code{"dotted"}, \code{"solid"} or \code{"noshow"}.
}
  \item{show_cluster_names}{
writes the names of the clusters on the picture. Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}; by default the names are not shown.
}
  \item{cluster_name_size}{
the size of the cluster names: \code{1}, \code{1.5}, \code{2}, \code{2.5}, \code{3}, \code{3.5}, \code{4}, \code{5} or \code{6}. By default STRING chooses the size that suits the picture.
}
}
\details{
A network of more than 1,000 proteins is drawn simplified, as on the STRING website: with confidence edges (whatever \code{network_flavor} says) and without structure pictures in the nodes.

STRING draws a PNG (\code{output_format="image"} or \code{"highres_image"}) of at most 150,000 interactions. For a larger network, and for any other request that STRING refuses, the method stops with STRING's explanation; for the PNG, ask for the SVG (\code{output_format="svg"}) or use a higher \code{required_score}.

The clustering options need STRING version 12.5 or later. An older version, such as \code{version="12.0"}, would ignore them, so they are not used there, with a warning.
}
\value{
For \code{output_format="image"} and \code{output_format="highres_image"}, returns a PNG image array. For \code{output_format="svg"}, returns the SVG markup as text.
}
\author{
Damian Szklarczyk
}
