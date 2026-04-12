\name{get_link}
\alias{\S4method{get_link}{STRINGdb}}
\title{
get_link
}
\description{
Returns a short link to the network page of the STRING website that shows the protein interactions between the given identifiers.
}
\usage{
\S4method{get_link}{STRINGdb}(string_ids=NULL, required_score=NULL, network_flavor="evidence", payload_id=NULL, network_term_id=NULL, hide_node_labels=NULL, hide_disconnected_nodes=NULL, block_structure_pics_in_bubbles=NULL, flat_node_design=TRUE, center_node_labels=NULL, custom_label_font_size=NULL, caller_identity="STRINGdb-package")
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
specify the flavor of the network ("evidence" or "confidence". Default "evidence").
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
}
\value{
short link to the network page of the STRING website that shows the protein interactions between the input identifiers.
}
\author{
Damian Szklarczyk
}
