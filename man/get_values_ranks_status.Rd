\name{get_values_ranks_status}
\alias{\S4method{get_values_ranks_status}{STRINGdb}}
\alias{get_values_ranks_status}
\title{
get_values_ranks_status
}
\description{
Returns the state of values/ranks jobs, with one request to STRING. It does not wait for the jobs.
}
\usage{
\S4method{get_values_ranks_status}{STRINGdb}(job_ids=NULL)
}
\arguments{
  \item{job_ids}{
job identifiers, e.g. from \code{submit_values_ranks_enrichment}. The default, \code{NULL}, lists all jobs of the API key, oldest first.
STRING keeps each job for the API key that submitted it (see \code{get_api_key}).
}
}
\value{
A data frame with one row per job and these columns:
\describe{
  \item{experiment}{the name of the job identifier}
  \item{job_id}{the job identifier}
  \item{status}{\code{"submitted"}, \code{"running"}, \code{"success"} or \code{"failed"}; \code{"unknown"} if this API key does not have the job}
  \item{message}{STRING's message}
  \item{creation_time}{when the job was submitted}
}
}
\seealso{
\code{\link{submit_values_ranks_enrichment}}, \code{\link{get_values_ranks_results}}
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
