\name{get_api_key}
\alias{\S4method{get_api_key}{STRINGdb}}
\alias{get_api_key}
\title{
get_api_key
}
\description{
Requests an API key from STRING for its values/ranks enrichment (\code{get_values_ranks_enrichment}), a GSEA-like enrichment analysis that is computed on STRING's servers, not locally. STRING gives the key without registration.
}
\usage{
\S4method{get_api_key}{STRINGdb}(caller_identity="STRINGdb-package")
}
\arguments{
  \item{caller_identity}{
caller identifier sent to STRING. A key requested with the default, \code{"STRINGdb-package"}, works at once.
}
}
\details{
Please request a key only once and keep it: store it as \code{STRINGDB_API_KEY} in your \code{~/.Renviron} file (for example with \code{usethis::edit_r_environ()})
and restart R, so that \code{get_values_ranks_enrichment} and \code{get_values_ranks_results} find it.
A key that this package requests with its default \code{caller_identity} works at once. STRING activates other new keys before it accepts them, for example a key requested with another \code{caller_identity} or outside R, which can take up to 15 minutes; until then, \code{get_values_ranks_enrichment} stops with a message that STRING is still activating the key.
}
\value{
The API key (invisibly). A message shows it, with STRING's note.
}
\seealso{
\code{get_values_ranks_enrichment}
}
\author{
Damian Szklarczyk
}
