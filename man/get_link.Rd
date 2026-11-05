\name{get_link}
\alias{\S4method{get_link}{STRINGdb}}
\title{
get_link
}
\description{
Returns a short link to the network page of the STRING website that shows the protein interactions between the given identifiers.
}
\usage{
\S4method{get_link}{STRINGdb}(string_ids=NULL, required_score=NULL, network_flavor="evidence", payload_id=NULL, network_term_id=NULL, hide_node_labels=NULL, hide_disconnected_nodes=NULL, block_structure_pics_in_bubbles=NULL, flat_node_design=TRUE, center_node_labels=NULL, custom_label_font_size=NULL, caller_identity="STRINGdb-package", typed_physical_edges=NULL, typed_regulatory_edges=NULL, show_regulatory_signs=NULL, add_color_nodes=NULL, add_white_nodes=NULL, show_query_node_labels=NULL, channels=NULL)
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
  \item{payload_id}{
an identifier of payload data on the STRING server (see method post_payload for additional informations).
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
  \item{add_color_nodes}{
adds this many interaction partners of the given proteins to the network, drawn in colour. A whole number of at least 0; the default \code{NULL} adds none.
}
  \item{add_white_nodes}{
adds this many further interaction partners to the network, drawn white, after the coloured ones. A whole number of at least 0; the default \code{NULL} adds none. Neither \code{add_color_nodes} nor \code{add_white_nodes} is used with \code{network_term_id}: STRING then adds no interaction partners.
}
  \item{show_query_node_labels}{
labels the given proteins with the identifiers submitted in \code{string_ids} instead of their STRING names (with STRING identifiers, the labels are those identifiers, e.g. \code{9606.ENSP00000269305}). Accepts \code{TRUE}/\code{FALSE} or \code{0}/\code{1}; by default the STRING names are shown.
}
  \item{channels}{
the evidence channels used for the interactions: a character vector of one or more of \code{"neighborhood"}, \code{"fusion"}, \code{"cooccurrence"}, \code{"coexpression"}, \code{"experiments"}, \code{"databases"} and \code{"textmining"} (in any case). The default \code{NULL} uses all channels. STRING recomputes the combined score of each interaction from the selected channels, and \code{required_score} applies to the recomputed score.
}
}
\details{
The \code{channels} option needs STRING version 12.0 or later. Older versions (11.5, 11.0b and 11.0) would ignore it, so it is not used there, with a warning, and the network page uses all channels.
}
\value{
short link to the network page of the STRING website that shows the protein interactions between the input identifiers.
}
\author{
Damian Szklarczyk
}
