\name{get_api_key}
\alias{\S4method{get_api_key}{STRINGdb}}
\alias{get_api_key}
\title{
get_api_key
}
\description{
Returns the STRING API key of this computer. The values/ranks methods use this key.
STRING gives the key without registration.
}
\usage{
\S4method{get_api_key}{STRINGdb}()
}
\details{
The key is taken from, in this order:
\enumerate{
  \item the environment variable \code{STRINGDB_API_KEY}, if it is set;
  \item the key saved in \code{tools::R_user_dir("STRINGdb", which = "config")}, readable only by you;
  \item a new key, which the package requests from STRING and saves.
}
The values/ranks methods find the key in the same way. You need \code{get_api_key} only to see the key.

To use the key on other computers, e.g. a cluster, set \code{STRINGDB_API_KEY} there.
To read the jobs of another key, set it in the R session: \code{Sys.setenv(STRINGDB_API_KEY = "...")}.
Keep the key private: anyone with it can list your jobs and read their results.

A key requested by the package works at once. Other new keys can take up to 15 minutes to be activated.
}
\value{
The API key (invisibly). A message shows the key and where it comes from.
}
\seealso{
\code{\link{get_values_ranks_enrichment}}, \code{\link{submit_values_ranks_enrichment}}
}
\examples{
if (interactive()) {
  string_db <- STRINGdb$new(version = "12.5", species = 9606)
  # the key of this computer (requested and saved the first time)
  string_db$get_api_key()
}
}
\author{
Damian Szklarczyk
}
