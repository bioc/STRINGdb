\name{plot_network}
\alias{\S4method{plot_network}{STRINGdb}}
\title{
plot_network
}
\description{
Plots an image of the STRING network with the given proteins.
}
\usage{
\S4method{plot_network}{STRINGdb}(string_ids, payload_id=NULL, required_score=NULL, add_link=FALSE, network_flavor="evidence", add_summary=TRUE, typed_physical_edges=NULL, typed_regulatory_edges=NULL, show_regulatory_signs=NULL)
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
}
\details{
A network of more than 1,000 proteins is drawn simplified, as on the STRING website: with confidence edges (whatever \code{network_flavor} says) and without structure pictures in the nodes.

The picture is a PNG, which STRING draws for at most 150,000 interactions. For a larger network the method stops with STRING's explanation: use a higher \code{required_score}, or \code{get_png} with \code{output_format="svg"}.
}
\author{
Andrea Franceschini
}
