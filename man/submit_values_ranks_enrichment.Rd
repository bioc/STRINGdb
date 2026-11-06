\name{submit_values_ranks_enrichment}
\alias{\S4method{submit_values_ranks_enrichment}{STRINGdb}}
\alias{submit_values_ranks_enrichment}
\title{
submit_values_ranks_enrichment
}
\description{
Submits STRING's values/ranks enrichment, the GSEA-like analysis of \code{get_values_ranks_enrichment}, and returns at once.
The R session is not blocked while STRING computes. This suits many experiments, e.g. tens or hundreds of columns.
See the progress with \code{get_values_ranks_status}. Get the results later with \code{get_values_ranks_results}, also in a new R session.
}
\usage{
\S4method{submit_values_ranks_enrichment}{STRINGdb}(string_ids, values, max_fdr=0.05, rank_direction="descending", quiet=FALSE)
}
\arguments{
  \item{string_ids}{
STRING identifiers, one per value. See \code{get_values_ranks_enrichment}.
}
  \item{values}{
the values to rank: a vector, or a table with one column per experiment. See \code{get_values_ranks_enrichment}.
}
  \item{max_fdr}{
the FDR cutoff of the reported terms. Default 0.05, at most 0.25.
}
  \item{rank_direction}{
\code{"descending"} (default) or \code{"ascending"}. See \code{get_values_ranks_enrichment}.
}
  \item{quiet}{
\code{TRUE} to print no progress messages.
}
}
\details{
Each experiment becomes a job. STRING runs up to 1000 jobs per API key at once.
Experiments beyond this limit are not submitted, with a warning. Submit them later with \code{values[, attr(job_ids, "not_submitted"), drop = FALSE]}.
For a vector of values, a refusal by STRING is an error.
The method needs STRING version 12.0 or later. It uses the API key of this computer (see \code{get_api_key}).
}
\value{
For a vector of values: the job identifier. For a table: the job identifiers, named by experiment.
The attribute \code{not_submitted} gives the columns of the experiments that were not submitted, if any.
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

  # submit and return at once; the jobs run side by side on STRING's servers
  job_ids <- string_db$submit_values_ranks_enrichment(mapped$STRING_id, rankings)
  print(string_db$get_values_ranks_status(job_ids))

  # later, also in a new R session on this computer
  results <- string_db$get_values_ranks_results(job_ids)
}
}
\author{
Damian Szklarczyk
}
