\name{plot_network}
\alias{\S4method{plot_network}{STRINGdb}}
\title{
plot_network
}
\description{
Plots an image of the STRING network with the given proteins.
}
\usage{
\S4method{plot_network}{STRINGdb}(string_ids, payload_id=NULL, required_score=NULL, add_link=FALSE, network_flavor="evidence", add_summary=TRUE, typed_physical_edges=NULL, typed_regulatory_edges=NULL, show_regulatory_signs=NULL, add_color_nodes=NULL, add_white_nodes=NULL, channels=NULL)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers
}
  \item{payload_id}{
an identifier of payload data on the STRING server (see method post_payload for additional informations)
}
  \item{required_score}{
a threshold on the score that overrides the default score_threshold, that we use only for the picture
}
  \item{add_link}{
parameter to specify whether you want to generate and add a short link to the relative page in STRING. 
The default is \code{FALSE}; disabling this option avoids generating and storing many short URLs when plotting in a loop.
}
  \item{network_flavor}{
style of network edges: \code{"evidence"}, \code{"confidence"}, or \code{"typed"}. The typed view is available only with \code{network_type="full"} (or \code{"functional"}). \code{"actions"} is accepted as a compatibility alias for \code{"typed"}.
}
  \item{add_summary}{
parameter to specify whether you want to add a summary text to the picture. This summary includes a p-value and the number of proteins/interactions.
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
adds this many further interaction partners to the network, drawn white, after the coloured ones. A whole number of at least 0. The default \code{NULL} leaves it to STRING, which adds none, or 10 white partners when a single protein is given. Giving \code{add_color_nodes} turns off those 10 white partners unless \code{add_white_nodes} is given too; \code{add_white_nodes} sets their number.
}
  \item{channels}{
the evidence channels used for the interactions: a character vector of one or more of \code{"neighborhood"}, \code{"fusion"}, \code{"cooccurrence"}, \code{"coexpression"}, \code{"experiments"}, \code{"databases"} and \code{"textmining"} (in any case), or the column names of the interaction tables with \code{link_data = "detailed"}: \code{"cooccurence"}, \code{"experimental"} and \code{"database"}. The default \code{NULL} uses all channels. STRING recomputes the combined score of each interaction from the selected channels, and \code{required_score} applies to the recomputed score. Needs STRING version 12.0 or later; with an older version it is not used, with a warning.
}
}
\details{
A network of more than 1,000 proteins is drawn simplified, as on the STRING website: with confidence edges (whatever \code{network_flavor} says) and without structure pictures in the nodes.

The picture is a PNG, which STRING draws for at most 150,000 interactions. For a larger network the method stops with STRING's explanation: use a higher \code{required_score}, or \code{get_png} with \code{output_format="svg"}.

\code{add_color_nodes}, \code{add_white_nodes} and \code{channels} are passed on to \code{get_png}, and to \code{get_link} for \code{add_link=TRUE}. With \code{add_color_nodes=NULL} the link adds no interaction partners, while the picture of a single protein shows STRING's 10 white partners; give \code{add_color_nodes} (and \code{add_white_nodes}) to get the same network in the picture and on the linked page. The summary text (\code{add_summary}) is computed with all channels, whatever \code{channels} says, and for the given proteins only, without the interaction partners that \code{add_color_nodes} and \code{add_white_nodes} add.
}
\author{
Andrea Franceschini
}
