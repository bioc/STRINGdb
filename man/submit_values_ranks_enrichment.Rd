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
\S4method{submit_values_ranks_enrichment}{STRINGdb}(string_ids, values, max_fdr=0.05, rank_direction="descending", quiet=FALSE)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers, one for each value, or, without \code{values}, the values alone; see \code{get_values_ranks_enrichment}.
}
  \item{values}{
a numeric vector (one experiment), or a matrix or data frame with one numeric column for each experiment and one row for each identifier. The values can also be given alone: a numeric vector named by STRING identifiers, or a table with STRING identifiers as row names; see \code{get_values_ranks_enrichment}.
}
  \item{max_fdr}{
the false discovery rate cutoff of the reported terms: above 0 and at most 0.25 (default 0.05).
}
  \item{rank_direction}{
\code{"descending"} (default) or \code{"ascending"}; see \code{get_values_ranks_enrichment}.
}
  \item{quiet}{
\code{TRUE} to print no progress messages.
}
}
\details{
The method needs STRING version 12.0 or later. It submits the jobs with the STRING API key of this computer (see \code{get_api_key}), and their results are read with the same key. Up to 1000 jobs of an API key may wait or run on STRING at once.
When STRING refuses a job because of this limit (or because its queue is full, or refuses the API key), or a submission request still fails after a few tries, the method stops submitting and returns the job identifiers of the experiments submitted so far, with a warning.
The column numbers of the other experiments, named by experiment, are the attribute \code{not_submitted} of the job identifiers: submit them again once jobs have finished, with \code{values[, attr(job_ids, "not_submitted"), drop = FALSE]} (name the columns of \code{values} to keep the names of the experiments).
\code{get_values_ranks_enrichment} instead submits experiments refused for the limit by itself as jobs finish, and waits for all results.
For a vector of values, a refusal by STRING (also for the limit) is an error with STRING's reason.
For a table, a refusal of the first job (e.g. for a wrong species) is an error, and so is a table of which STRING takes no job; a later experiment that STRING refuses for another reason is named in a warning, and is neither among the job identifiers nor in \code{not_submitted}.
An interrupt with Ctrl-C stops the submissions, and a message lists the jobs submitted until then.
}
\value{
For a vector of values, the job identifier. For a table of values, the job identifiers, named by experiment, with the attribute \code{not_submitted} (column numbers, named by experiment) when some experiments were not submitted.
}
\seealso{
\code{get_values_ranks_status}, \code{get_values_ranks_results}, \code{get_values_ranks_enrichment}, \code{get_api_key}
}
\concept{GSEA}
\examples{
if (interactive()) {
  string_db <- STRINGdb$new(version = "12.5", species = 9606)
  data(diff_exp_example1)
  mapped <- string_db$map(diff_exp_example1, "gene", removeUnmappedRows = TRUE)
  rankings <- data.frame(logFC = mapped$logFC,
                         signed_p = sign(mapped$logFC) * -log10(mapped$pvalue))

  # submit without waiting; the jobs run side by side on STRING's servers
  job_ids <- string_db$submit_values_ranks_enrichment(mapped$STRING_id, rankings)
  print(string_db$get_values_ranks_status(job_ids))

  # later, also in a new R session on this computer
  results <- string_db$get_values_ranks_results(job_ids)
}
}
\author{
Damian Szklarczyk
}
