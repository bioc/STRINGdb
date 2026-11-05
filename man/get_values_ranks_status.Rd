\name{get_values_ranks_status}
\alias{\S4method{get_values_ranks_status}{STRINGdb}}
\alias{get_values_ranks_status}
\title{
get_values_ranks_status
}
\description{
Returns the state of values/ranks jobs, STRING's GSEA-like enrichment analysis that is computed on STRING's servers, not locally, without waiting: one request to STRING reports all of them. Without job identifiers, it lists all jobs of the API key, e.g. to find job identifiers that were lost. It needs STRING version 12.0 or later.
}
\usage{
\S4method{get_values_ranks_status}{STRINGdb}(job_ids=NULL)
}
\arguments{
  \item{job_ids}{
the job identifiers, named or not, e.g. what \code{submit_values_ranks_enrichment} returns. The default \code{NULL} lists all jobs of the API key, oldest first. STRING keeps each job for the API key that submitted it: the key of this computer, or another one given as \code{STRINGDB_API_KEY} (see \code{get_api_key}).
}
}
\value{
A data frame with one row for each job and the columns \code{experiment} (the name of the job identifier; \code{NA} when the job identifiers have no names, and the job identifier for an unnamed one among named ones), \code{job_id},
\code{status}, one of \code{"submitted"} (waiting in STRING's queue), \code{"running"}, \code{"success"} (finished: \code{get_values_ranks_results} returns the results), \code{"failed"} (the message gives STRING's reason; also a job that STRING no longer has) and \code{"unknown"} (STRING does not list the job for this key: it was removed, or another key submitted it),
\code{message} (STRING's) and \code{creation_time}.
}
\seealso{
\code{submit_values_ranks_enrichment}, \code{get_values_ranks_results}
}
\concept{GSEA}
\examples{
if (interactive()) {
  string_db <- STRINGdb$new(version = "12.5", species = 9606)
  data(diff_exp_example1)
  mapped <- string_db$map(diff_exp_example1, "gene", removeUnmappedRows = TRUE)
  job_id <- string_db$submit_values_ranks_enrichment(setNames(mapped$logFC, mapped$STRING_id))
  string_db$get_values_ranks_status(job_id)
}
}
\author{
Damian Szklarczyk
}
