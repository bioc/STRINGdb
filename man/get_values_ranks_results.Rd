\name{get_values_ranks_results}
\alias{\S4method{get_values_ranks_results}{STRINGdb}}
\alias{get_values_ranks_results}
\title{
get_values_ranks_results
}
\description{
Returns the results of values/ranks jobs, STRING's GSEA-like enrichment analysis of a ranked list, which runs on STRING's servers, not locally.
The jobs were submitted with \code{submit_values_ranks_enrichment} or \code{get_values_ranks_enrichment}; the method waits for the jobs that have not finished yet:
for example after \code{submit_values_ranks_enrichment}, after the wait of \code{get_values_ranks_enrichment} timed out or was interrupted, or in a new R session.
}
\usage{
\S4method{get_values_ranks_results}{STRINGdb}(job_ids, timeout=7200, api_key=NULL, quiet=FALSE, caller_identity="STRINGdb-package")
}
\arguments{
  \item{job_ids}{
one or more job identifiers, named or not: what \code{submit_values_ranks_enrichment} returns, the \code{job_ids} or \code{job_id} attribute of the results of \code{get_values_ranks_enrichment}, or those in its messages.
}
  \item{timeout}{
the number of seconds to wait for the jobs (default 7200).
}
  \item{api_key}{
the STRING API key that submitted the jobs (default \code{NULL}: the key of this computer; see \code{get_api_key}). STRING keeps each job for the key that submitted it.
}
  \item{quiet}{
\code{TRUE} to print no progress messages.
}
  \item{caller_identity}{
caller identifier sent to STRING.
}
}
\details{
One status request reports all jobs, and the results of each job are downloaded as soon as it has finished.
STRING keeps the results of a job for a limited time; after that, submit the job again.
}
\value{
For one job identifier without a name, the results of the job as \code{get_values_ranks_enrichment} returns them for a vector of values.
For several job identifiers, or named ones, a list of such results named by job (by the names of \code{job_ids}, or else by the job identifiers), and the job identifiers as the attribute \code{job_ids}.
A job has \code{NULL} when it failed (a warning gives STRING's reason) or did not finish before the timeout (a warning says so; ask again later with the same job identifiers).
For one job identifier, the timeout ends the wait with an error.
}
\seealso{
\code{submit_values_ranks_enrichment}, \code{get_values_ranks_status}, \code{get_values_ranks_enrichment}
}
\concept{GSEA}
\author{
Damian Szklarczyk
}
