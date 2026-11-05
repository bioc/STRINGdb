\name{get_api_key}
\alias{\S4method{get_api_key}{STRINGdb}}
\alias{get_api_key}
\title{
get_api_key
}
\description{
Returns the STRING API key of this computer for STRING's values/ranks enrichment (\code{get_values_ranks_enrichment}), a GSEA-like enrichment analysis that is computed on STRING's servers, not locally.
STRING gives the key without registration.
}
\usage{
\S4method{get_api_key}{STRINGdb}(caller_identity="STRINGdb-package", new=FALSE)
}
\arguments{
  \item{caller_identity}{
caller identifier sent to STRING. A key requested with the default, \code{"STRINGdb-package"}, works at once.
}
  \item{new}{
\code{TRUE} requests a new key from STRING and saves it in place of the saved key (default \code{FALSE}).
STRING keeps each job for the key that submitted it, so the jobs of the earlier key can no longer be read with the new one.
}
}
\details{
The key of this computer is the environment variable \code{STRINGDB_API_KEY} when it is set, otherwise the key that the package saved, otherwise a new key, which \code{get_api_key} requests from STRING and saves.
The values/ranks methods find the key in the same way, and request and save a key themselves when there is none, so \code{get_api_key} is needed only to see the key, e.g. to use it on other computers.
The key is saved in a file of the package's configuration directory, \code{tools::R_user_dir("STRINGdb", which = "config")}, readable only by the user; R sessions on the same computer, or with the same home directory, use the same key.
To use one key on several computers, e.g. on the nodes of a cluster, set \code{STRINGDB_API_KEY} there, for example in the \code{~/.Renviron} file.
When R sessions of one computer request its first key at the same time, all of them keep the key that was saved first.
When STRING no longer knows the saved key (it says so for a submission and again for a status request, so that a short outage of STRING does not count), the values/ranks methods request a new key, and save it in place of the old one once STRING has taken a job with it; a warning gives both keys.
\code{get_api_key(new = TRUE)} also shows the key that it replaces.

A key that the package requests with its default \code{caller_identity} works at once. STRING activates other new keys before it accepts them, for example a key requested with another \code{caller_identity} or outside R, which can take up to 15 minutes; until then, the values/ranks methods stop with a message that STRING is still activating the key.
}
\value{
The API key (invisibly). A message shows it, and where it comes from.
}
\seealso{
\code{get_values_ranks_enrichment}, \code{submit_values_ranks_enrichment}
}
\author{
Damian Szklarczyk
}
