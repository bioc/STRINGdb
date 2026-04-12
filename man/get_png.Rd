\name{get_png}
\alias{\S4method{get_png}{STRINGdb}}
\title{
get_png
}
\description{
Returns a STRING network image for the given identifiers or for a STRING functional term.
}
\usage{
\S4method{get_png}{STRINGdb}(string_ids=NULL, required_score=NULL, network_flavor="evidence", file=NULL, payload_id=NULL, output_format="image", network_term_id=NULL, hide_node_labels=NULL, hide_disconnected_nodes=NULL, block_structure_pics_in_bubbles=NULL, flat_node_design=TRUE, center_node_labels=NULL, custom_label_font_size=NULL, caller_identity="STRINGdb-package")
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
}
\value{
For \code{output_format="image"} and \code{output_format="highres_image"}, returns a PNG image array. For \code{output_format="svg"}, returns the SVG markup as text.
}
\author{
Damian Szklarczyk
}
