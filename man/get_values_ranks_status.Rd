\name{get_values_ranks_status}
\alias{\S4method{get_values_ranks_status}{STRINGdb}}
\alias{get_values_ranks_status}
\title{
get_values_ranks_status
}
\description{
Returns the state of values/ranks jobs, STRING's GSEA-like enrichment analysis that is computed on STRING's servers, not locally, without waiting: one request to STRING reports all of them.
}
\usage{
\S4method{get_values_ranks_status}{STRINGdb}(job_ids, api_key=NULL, caller_identity="STRINGdb-package")
}
\arguments{
  \item{job_ids}{
the job identifiers, named or not, e.g. what \code{submit_values_ranks_enrichment} returns.
}
  \item{api_key}{
the STRING API key that submitted the jobs (default \code{NULL}: the key of this computer; see \code{get_api_key}). STRING keeps each job for the key that submitted it.
}
  \item{caller_identity}{
caller identifier sent to STRING.
}
}
\value{
A data frame with one row for each job and the columns \code{experiment} (the name of the job identifier, \code{NA} without names), \code{job_id},
\code{status} (STRING's state of the job, e.g. \code{"submitted"}, \code{"running"}, \code{"success"} or \code{"error"}; \code{"unknown"} for a job that STRING does not list for this key),
\code{message} and \code{creation_time}.
}
\seealso{
\code{submit_values_ranks_enrichment}, \code{get_values_ranks_results}
}
\concept{GSEA}
\author{
Damian Szklarczyk
}
