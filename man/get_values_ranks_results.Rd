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
\S4method{get_values_ranks_results}{STRINGdb}(job_ids, timeout=7200, quiet=FALSE)
}
\arguments{
  \item{job_ids}{
one or more job identifiers, named or not: what \code{submit_values_ranks_enrichment} returns, the \code{job_ids} or \code{job_id} attribute of the results of \code{get_values_ranks_enrichment}, or those in its messages. STRING keeps each job for the API key that submitted it: the key of this computer, or another one given as \code{STRINGDB_API_KEY} (see \code{get_api_key}).
}
  \item{timeout}{
the number of seconds to wait for the jobs (default 7200).
}
  \item{quiet}{
\code{TRUE} to print no progress messages.
}
}
\details{
The method needs STRING version 12.0 or later.
One status request reports all jobs, and the results of each job are downloaded as soon as it has finished; a download that fails is tried again a few times.
STRING keeps the results of a job for a limited time; after that, submit the job again.
}
\value{
The results have the form that \code{get_values_ranks_enrichment} gives for the same values: \code{submit_values_ranks_enrichment} returns one job identifier without a name for a vector of values, whose results are a data frame, and job identifiers named by experiment for a table of values (also of one column), whose results are a list.
In detail: for one job identifier without a name, the results of the job as \code{get_values_ranks_enrichment} returns them for a vector of values.
For several job identifiers, or named ones, a list of such results named by job (by the names of \code{job_ids}, or else by the job identifiers), with the attributes \code{status} (for each job \code{"success"}, \code{"failed"} or \code{"unfinished"}) and \code{job_ids}.
A job has \code{NULL} when it failed (a warning gives STRING's reason) or did not finish before the timeout (a warning says so; ask again later with the same job identifiers).
For one job identifier without a name, a failed job or the timeout ends with an error.
}
\seealso{
\code{submit_values_ranks_enrichment}, \code{get_values_ranks_status}, \code{get_values_ranks_enrichment}
}
\concept{GSEA}
\examples{
if (interactive()) {
  string_db <- STRINGdb$new(version = "12.5", species = 9606)
  data(diff_exp_example1)
  mapped <- string_db$map(diff_exp_example1, "gene", removeUnmappedRows = TRUE)
  job_id <- string_db$submit_values_ranks_enrichment(setNames(mapped$logFC, mapped$STRING_id))

  # waits for the job if it has not finished yet
  result <- string_db$get_values_ranks_results(job_id)
  head(result[, c("category", "description", "direction", "fdr")])
}
}
\author{
Damian Szklarczyk
}
