\name{get_values_ranks_enrichment}
\alias{\S4method{get_values_ranks_enrichment}{STRINGdb}}
\alias{get_values_ranks_enrichment}
\title{
get_values_ranks_enrichment
}
\description{
A GSEA-like enrichment analysis of a ranked list of proteins.
It is STRING's "Proteins with Values/Ranks" analysis, computed on STRING's servers, not locally.
Give all the proteins measured in the experiment, with their values, without a cutoff: the submitted list is the background of the test.

STRING tests larger terms, and terms with a clear signal, with a Kolmogorov-Smirnov (KS) test. It tests the other terms with the more sensitive, but slower, aggregate fold change (AFC) test.
The method reports terms enriched at the top or at the bottom of the list.
It also reports terms enriched at both ends: terms whose proteins are partly up- and partly down-regulated.

The method waits until STRING has finished. Then it returns the results as a data frame.
A table of values, with one column per experiment, returns a list of data frames.
For many experiments, e.g. tens or hundreds of columns, do not block the R session: submit them with \code{submit_values_ranks_enrichment}, and get the results later with \code{get_values_ranks_results}.
}
\usage{
\S4method{get_values_ranks_enrichment}{STRINGdb}(string_ids, values, max_fdr=0.05, rank_direction="descending", timeout=7200, quiet=FALSE)
}
\arguments{
  \item{string_ids}{
STRING identifiers, one per value, e.g. the \code{STRING_id} column of \code{\link{map}}.
}
  \item{values}{
the values to rank, for all the proteins measured. Give signed values, such as log fold changes, t statistics or \code{sign(logFC) * -log10(pvalue)}; not p-values, which lose the direction.
A numeric vector, or a matrix or data frame with one column per experiment.
The values can also be given alone, without \code{string_ids}: a vector named by STRING identifiers, or a table with them as row names. Then give the other arguments by name.
}
  \item{max_fdr}{
the FDR cutoff of the reported terms. Default 0.05, at most 0.25.
}
  \item{rank_direction}{
\code{"descending"} (default) or \code{"ascending"}. With \code{"descending"}, rank 1 and the "top" are the largest values. It only names the ends of the list; the enrichment is the same.
}
  \item{timeout}{
the time each job may take on STRING, in seconds, from its submission. Default 7200. The time an experiment waits to be submitted does not count.
}
  \item{quiet}{
\code{TRUE} to print no progress messages. The messages name each experiment: its column of the table, or, for a vector, the values as you gave them (e.g. \code{mapped$logFC}).
}
}
\details{
The FDR is the Benjamini-Hochberg FDR within each category.
STRING's statistics differ from those of fgsea or clusterProfiler, so their scores and FDRs cannot be compared.

\code{string_ids} and \code{values} are paired by position.
Rows without an identifier or a value are left out. Infinite values are left out with a warning.
A repeated identifier keeps its value farthest from 0.

The method needs STRING version 12.0 or later. It uses the API key of this computer (see \code{\link{get_api_key}}). \code{set_background} does not apply: the submitted proteins are the background.

With a table, STRING runs up to 1000 jobs per API key at once. The others are submitted as jobs finish. An experiment that STRING cannot take within the timeout is not submitted. A job that fails does not stop the others.
If the wait ends early (timeout, interrupt or lost connection), the jobs keep running on STRING. Get their results later with \code{get_values_ranks_results}.
For a vector of values, a timeout is an error, and its message gives the job identifier. For a table, the experiments that did not finish have the status \code{"unfinished"}.
}
\value{
A data frame with one row per enriched term. The rows are ordered by category, then by FDR, as in \code{get_enrichment}.
Its columns are those of \code{\link{get_enrichment}} where they mean the same:
\describe{
  \item{category}{the category of the term, with the codes of \code{get_enrichment}, e.g. \code{"Process"} or \code{"RCTM"}}
  \item{term}{the identifier of the term}
  \item{number_of_genes}{the number of the term's proteins in the input}
  \item{number_of_genes_in_background}{the number of the term's proteins in the genome, for information only: the test uses only the input}
  \item{inputGenes, preferredNames}{the STRING identifiers and the names of all the term's proteins in the input, wherever they rank: this is not a leading edge. Comma-separated; a comma inside a name is \code{&comma;}.}
  \item{enrichment_score}{how far the mean value of the term's proteins lies from the mean of all values, relative to the most extreme value, times 10: from 0 (the mean) to 10 (the most extreme value). Real scores are usually far below 10. The score has no sign (see \code{direction}), and it is not GSEA's ES or NES.}
  \item{direction}{\code{"top"}, \code{"bottom"} or \code{"both ends"}}
  \item{method}{the test: \code{"ks"} or \code{"afc"}}
  \item{fdr}{the false discovery rate}
  \item{description}{the description of the term}
  \item{inputValues, inputRanks}{list columns with the values of the term's proteins and their ranks in the whole list, in the order of \code{inputGenes}. Paste them into text before \code{write.csv}, e.g. \code{result$inputValues <- vapply(result$inputValues, paste, "", collapse = ",")}, and the same for \code{inputRanks}.}
}
The attributes \code{job_id}, \code{page_url}, \code{graph_url}, \code{download_url} and \code{string_version} give the job and its pages on the STRING website.
In the next Bioconductor release, \code{inputGenes} and \code{preferredNames} will become list columns too.

For a table of values: a list of such data frames, named by experiment.
Its attribute \code{status} gives the state of each experiment: \code{"success"}, \code{"failed"}, \code{"unfinished"} or \code{"not submitted"}.
An experiment without success has \code{NULL}, and a warning says why.
The attribute \code{job_ids} gives the jobs. The attribute \code{not_submitted} gives the columns of the experiments that were not submitted.
}
\seealso{
\code{\link{submit_values_ranks_enrichment}}, \code{\link{get_values_ranks_status}}, \code{\link{get_values_ranks_results}}, \code{\link{get_api_key}}, \code{\link{get_enrichment}}, \code{\link{map}}
}
\references{
The values/ranks enrichment: Szklarczyk D, et al. (2019). STRING v11: protein-protein association networks with increased coverage, supporting functional discovery in genome-wide experimental datasets. Nucleic Acids Res 47(D1):D607-D613.

GSEA, which this analysis resembles: Subramanian A, et al. (2005). Gene set enrichment analysis: a knowledge-based approach for interpreting genome-wide expression profiles. Proc Natl Acad Sci USA 102(43):15545-15550.
}
\examples{
if (interactive()) {
  string_db <- STRINGdb$new(version = "12.5", species = 9606)
  data(diff_exp_example1)
  mapped <- string_db$map(diff_exp_example1, "gene", removeUnmappedRows = TRUE)

  # one experiment: the values named by STRING identifiers, as for fgsea or clusterProfiler
  ranked <- setNames(mapped$logFC, mapped$STRING_id)
  result <- string_db$get_values_ranks_enrichment(ranked, max_fdr = 0.01)
  # the strongest terms first: the rows are ordered by category, then by FDR
  top_terms <- result[order(result$fdr), ]
  print(head(top_terms[, c("category", "description", "direction", "enrichment_score", "fdr")]))

  # several experiments at once: one column each, run side by side on STRING's servers
  rankings <- data.frame(logFC = mapped$logFC,
                         signed_p = sign(mapped$logFC) * -log10(mapped$pvalue))
  results <- string_db$get_values_ranks_enrichment(mapped$STRING_id, rankings, max_fdr = 0.01)
  print(attr(results, "status"))
  sapply(results[attr(results, "status") == "success"], nrow)
}
}
\concept{GSEA}
\concept{gene set enrichment analysis}
\concept{ranked list enrichment}
\author{
Damian Szklarczyk
}
