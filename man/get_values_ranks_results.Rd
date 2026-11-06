\name{get_values_ranks_results}
\alias{\S4method{get_values_ranks_results}{STRINGdb}}
\alias{get_values_ranks_results}
\title{
get_values_ranks_results
}
\description{
Returns the results of values/ranks jobs. It waits for the jobs that have not finished.
Use it after \code{submit_values_ranks_enrichment}, or to get results in a new R session.
}
\usage{
\S4method{get_values_ranks_results}{STRINGdb}(job_ids, timeout=7200, quiet=FALSE)
}
\arguments{
  \item{job_ids}{
job identifiers, e.g. from \code{submit_values_ranks_enrichment}.
STRING keeps each job for the API key that submitted it (see \code{get_api_key}).
}
  \item{timeout}{
the time to wait for each job, in seconds. Default 7200.
}
  \item{quiet}{
\code{TRUE} to print no progress messages.
}
}
\details{
STRING keeps the results of a job for a limited time. After that, submit the job again.

In another R session, give the job identifiers that \code{submit_values_ranks_enrichment} returned, e.g. \code{readRDS("jobs.rds")}: their names give the experiments, and the results are named by them.
}
\value{
The results have the same form as from \code{get_values_ranks_enrichment}:
\itemize{
  \item One job identifier without a name (from a vector of values) gives a data frame. A failed job or the timeout is an error.
  \item Job identifiers named by experiment (from a table) give a list of data frames, with the attributes \code{status} (\code{"success"}, \code{"failed"} or \code{"unfinished"}) and \code{job_ids}. A job without success has \code{NULL}, and a warning says why.
}
}
\seealso{
\code{\link{submit_values_ranks_enrichment}}, \code{\link{get_values_ranks_status}}, \code{\link{get_values_ranks_enrichment}}
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
