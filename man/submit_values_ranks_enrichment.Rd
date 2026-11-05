\name{submit_values_ranks_enrichment}
\alias{\S4method{submit_values_ranks_enrichment}{STRINGdb}}
\alias{submit_values_ranks_enrichment}
\title{
submit_values_ranks_enrichment
}
\description{
Submits STRING's values/ranks enrichment, the GSEA-like analysis of \code{get_values_ranks_enrichment} that is computed on STRING's servers, not locally, without waiting for it.
It submits one job for each experiment, which STRING's queue runs side by side, and returns their job identifiers at once.
Get the results later with \code{get_values_ranks_results}, also in a new R session, and see the progress with \code{get_values_ranks_status}.
}
\usage{
\S4method{submit_values_ranks_enrichment}{STRINGdb}(string_ids, values, max_fdr=0.01, rank_direction="descending", api_key=NULL, quiet=FALSE, caller_identity="STRINGdb-package")
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers, one for each value; see \code{get_values_ranks_enrichment}.
}
  \item{values}{
a numeric vector (one experiment), or a matrix or data frame with one numeric column for each experiment and one row for each identifier; see \code{get_values_ranks_enrichment}.
}
  \item{max_fdr}{
the false discovery rate cutoff of the reported terms: above 0 and at most 0.25 (default 0.01).
}
  \item{rank_direction}{
\code{"descending"} (default) or \code{"ascending"}; see \code{get_values_ranks_enrichment}.
}
  \item{api_key}{
a STRING API key (default \code{NULL}: the key of this computer; see \code{get_api_key}). The results are read with the same key.
}
  \item{quiet}{
\code{TRUE} to print no progress messages.
}
  \item{caller_identity}{
caller identifier sent to STRING.
}
}
\details{
Up to 1000 jobs of an API key may wait or run on STRING at once.
When STRING refuses a job because of this limit (or because its queue is full, or refuses the API key), or the submissions keep failing, the method stops submitting and returns the job identifiers of the experiments submitted so far, with a warning.
The column numbers of the other experiments, named by experiment, are the attribute \code{not_submitted} of the job identifiers: submit them again once jobs have finished, with \code{values[, attr(job_ids, "not_submitted"), drop = FALSE]} (name the columns of \code{values} to keep the names of the experiments).
\code{get_values_ranks_enrichment} instead submits experiments refused for the limit by itself as jobs finish, and waits for all results.
An interrupt with Ctrl-C during the submissions ends with an error whose message lists the jobs submitted until then.
}
\value{
For a vector of values, the job identifier. For a table of values, the job identifiers, named by experiment, with the attribute \code{not_submitted} (column numbers, named by experiment) when some experiments were not submitted.
}
\seealso{
\code{get_values_ranks_status}, \code{get_values_ranks_results}, \code{get_values_ranks_enrichment}, \code{get_api_key}
}
\concept{GSEA}
\author{
Damian Szklarczyk
}
