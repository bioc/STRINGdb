\name{get_api_key}
\alias{\S4method{get_api_key}{STRINGdb}}
\alias{get_api_key}
\title{
get_api_key
}
\description{
Requests an API key from STRING for its values/ranks enrichment (\code{get_gsea}). STRING gives the key without registration.
}
\usage{
\S4method{get_api_key}{STRINGdb}(caller_identity="STRINGdb-package")
}
\arguments{
  \item{caller_identity}{
caller identifier sent to STRING.
}
}
\details{
Please request a key only once and keep it: store it as \code{STRINGDB_API_KEY} in your \code{~/.Renviron} file (for example with \code{usethis::edit_r_environ()})
and restart R, so that \code{get_gsea} and \code{get_gsea_results} find it. STRING activates a new key before it accepts it, which can take up to an hour; until then, \code{get_gsea} stops with a message that STRING is still activating the key.
}
\value{
The API key (invisibly). A message shows it, with STRING's note.
}
\seealso{
\code{get_gsea}
}
\author{
Damian Szklarczyk
}
