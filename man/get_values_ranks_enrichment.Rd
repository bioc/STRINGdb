\name{get_values_ranks_enrichment}
\alias{\S4method{get_values_ranks_enrichment}{STRINGdb}}
\alias{get_values_ranks_enrichment}
\title{
get_values_ranks_enrichment
}
\description{
A GSEA-like gene set enrichment analysis of a whole ranked experiment, computed on STRING's servers, not locally: STRING's values/ranks enrichment ("Proteins with Values/Ranks" on the STRING website).
The method submits a job to STRING's job queue, waits for it and downloads the results.
Give it all measured proteins with their values, e.g. log fold changes, without any cutoff: it finds the terms (pathways, GO terms and the other categories of STRING) whose proteins gather at the top, at the bottom or at both ends of the list ranked by these values.
Several experiments run at once: give a table of values, with one column for each experiment.
}
\usage{
\S4method{get_values_ranks_enrichment}{STRINGdb}(string_ids, values, max_fdr=0.05, rank_direction="descending", timeout=7200, quiet=FALSE)
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers (e.g. the \code{STRING_id} column of \code{map}), one for each value.
Submit the whole experiment, e.g. all measured proteins, without any cutoff.
\code{string_ids} and \code{values} are paired by position. Rows without an identifier or without a value are left out; infinite values (e.g. -log10 of a p-value of 0) are left out with a warning. An identifier given more than once keeps its value farthest from 0, so give signed values, such as log fold changes or t statistics, not p-values.
Without \code{values}, the values alone (see \code{values}).
}
  \item{values}{
the values to rank, e.g. log fold changes or t statistics: a numeric vector, or a matrix or data frame with one numeric column for each experiment (named by its column; an unnamed column is named \code{experiment_1}, \code{experiment_2}, ...) and one row for each identifier of \code{string_ids}.
In a table, a missing value (\code{NA}) leaves the protein out of that experiment only.
Values named by STRING identifiers in another order than \code{string_ids} stop with an error, as the two are paired by position: give such values alone.
The values can also be given alone, without \code{string_ids}, as R's GSEA packages take them (e.g. the \code{stats} of fgsea): a numeric vector named by STRING identifiers, such as \code{setNames(mapped$logFC, mapped$STRING_id)} or \code{tapply(mapped$logFC, mapped$STRING_id, mean)}, or a matrix or data frame with STRING identifiers as row names and one column for each experiment.
Row names that are only the numbers of the rows, as a data frame keeps them after subsetting, are not identifiers and stop. With the values alone, give the other arguments by name.
}
  \item{max_fdr}{
the false discovery rate cutoff of the reported terms: above 0 and at most 0.25 (default 0.05).
}
  \item{rank_direction}{
\code{"descending"} (default: rank 1 is the largest value, and \code{"top"} is the end of the largest values) or \code{"ascending"} (rank 1 is the smallest value).
It only names the two ends of the ranked list and numbers the ranks; the enrichment itself is the same.
}
  \item{timeout}{
the number of seconds to wait for the jobs (default 7200).
}
  \item{quiet}{
\code{TRUE} to print no progress messages.
}
}
\details{
The values/ranks enrichment is a GSEA in purpose: like \code{get_enrichment} it tests the terms of STRING's categories, but against the whole ranked list instead of a selected set of proteins. A background set with \code{set_background} does not apply. The method needs STRING version 12.0 or later.
Its statistics differ from the GSEA of Subramanian et al. (2005), as implemented for example in the fgsea and clusterProfiler packages, which computes a weighted running-sum enrichment score, normalizes it (NES) and assesses it by permutations.
STRING tests each term with a Kolmogorov-Smirnov test of the ranks of its proteins, and a small term with a borderline result also with a permutation-based aggregate fold change test (column \code{method}: \code{"ks"} or \code{"afc"}).
A term can be enriched at the top, at the bottom or at both ends of the list; terms enriched in the centre of the list are not reported.
The false discovery rate is the Benjamini-Hochberg FDR within each category, and \code{enrichment_score} is STRING's effect strength (see Value), not the enrichment score of that GSEA: the scores and FDRs of the two methods cannot be compared.

The identifiers and values are sent to STRING. The method checks the state of the jobs every few seconds (from 5 s, growing to 30 s) and downloads the results of each job as soon as it has finished, as STRING keeps them only for a limited time; a download that fails is tried again a few times.
When the wait ends early, the jobs keep running on STRING; get their results later with \code{get_values_ranks_results}.
For a vector of values, the timeout or a lost connection ends the wait with an error whose message gives the job identifier.
For a table of values, the timeout returns the results so far (see Value), and a lost connection ends with an error after a message that lists the job identifiers (more than 20 are saved in a file of the R session, which the message names).
An interrupt with Ctrl-C stops the wait, and a message gives the job identifiers in the same way.
The method uses the STRING API key of this computer: the environment variable \code{STRINGDB_API_KEY} when it is set, otherwise the key that the package saved, otherwise a new key, which it requests from STRING and saves (see \code{get_api_key}). STRING keeps each job for the API key that submitted it, so \code{get_values_ranks_results} needs the same key: on the same computer it is the same; on another computer, give it as \code{STRINGDB_API_KEY}.

\strong{Many experiments at once.} With a table of values, the method submits one job for each experiment before it waits, so that STRING's queue runs them side by side on STRING's servers, and returns all results.
Up to 1000 jobs of an API key may wait or run on STRING at once: when STRING refuses a job because of this limit (also the job of a vector of values), the method submits it as soon as other jobs have finished, within the timeout.
A job that fails does not stop the others, but a refusal of the first job (e.g. for a wrong species or API key) stops the call with STRING's error, as it concerns all experiments. To submit without waiting, e.g. many experiments whose results are collected later or in another R session, use \code{submit_values_ranks_enrichment}, \code{get_values_ranks_status} and \code{get_values_ranks_results}.
}
\value{
For a vector of values, a data frame with one row for each enriched term, whose columns are those of \code{get_enrichment} where they mean the same, so that code that selects or filters by these columns works for the results of both (\code{get_enrichment}'s \code{p_value}, \code{strength}, \code{signal} and \code{ncbiTaxonId} have no counterpart here; \code{fdr} is the significance):
\code{category} (the category codes of \code{get_enrichment}, such as \code{"Process"}, \code{"RCTM"} or \code{"NetworkNeighborAL"}), \code{term}, \code{number_of_genes} (proteins of the term in the input), \code{number_of_genes_in_background} (proteins of the term in the genome),
\code{inputGenes} and \code{preferredNames} (the STRING identifiers and names of the term's proteins in the input, as comma-separated text as in \code{get_enrichment}; STRING writes a comma inside a name as \code{&comma;}),
then \code{enrichment_score}, \code{direction} (\code{"top"}, \code{"bottom"} or \code{"both ends"}), \code{method} (\code{"ks"} or \code{"afc"}), \code{fdr} and \code{description},
and the list columns \code{inputValues} (numeric) and \code{inputRanks} (integer): the values and the ranks of the term's proteins, in the order of \code{inputGenes}.
The data frame has the attributes \code{job_id}, \code{page_url} (the results on the STRING website), \code{graph_url} (STRING's enrichment figure), \code{download_url} and \code{string_version}.
To write the results to a file with \code{write.csv} or \code{write.table}, which take no list columns, first make them text, e.g. \code{result$inputValues <- vapply(result$inputValues, paste, character(1), collapse = ",")}, and the same for \code{inputRanks}.
In the next Bioconductor release, \code{inputGenes} and \code{preferredNames} will become list columns too (a character vector for each term), here and in \code{get_enrichment}.

For a table of values, a list of such data frames named by experiment, with the attributes \code{status} and \code{job_ids} (the job identifiers, named by experiment).
\code{status} gives the state of each experiment: \code{"success"}, \code{"failed"} (its job failed or STRING refused it; a warning gives STRING's reason), \code{"unfinished"} (the timeout ended the wait before its job finished) or \code{"not submitted"} (the timeout came first, the submissions to STRING kept failing, or STRING refused the API key; a warning says so).
An experiment that is not \code{"success"} has \code{NULL}, which \code{status} tells from results without terms.
\code{get_values_ranks_results(attr(results, "job_ids"))} returns the results of all submitted jobs later, and the column numbers of the experiments that were not submitted, named by experiment, are the attribute \code{not_submitted}, so that \code{values[, attr(results, "not_submitted"), drop = FALSE]} are their values.

\code{enrichment_score} tells how far the values of the term's proteins lie towards their end of the list, from 0 to 10: 10 times the difference between their mean and the mean of all values, divided by the difference between the most extreme value at that end and the mean of all values.
For a term at both ends, the same is computed for the distances of the values from the mean of all values.
}
\seealso{
\code{submit_values_ranks_enrichment}, \code{get_values_ranks_status}, \code{get_values_ranks_results}, \code{get_api_key}, \code{get_enrichment}, \code{map}
}
\references{
Subramanian A, et al. (2005). Gene set enrichment analysis: a knowledge-based approach for interpreting genome-wide expression profiles. Proc Natl Acad Sci USA 102(43):15545-15550.
}
\examples{
if (interactive()) {
  string_db <- STRINGdb$new(version = "12.5", species = 9606)
  data(diff_exp_example1)
  mapped <- string_db$map(diff_exp_example1, "gene", removeUnmappedRows = TRUE)

  # one experiment: the values named by STRING identifiers, as for fgsea or clusterProfiler
  ranked <- setNames(mapped$logFC, mapped$STRING_id)
  result <- string_db$get_values_ranks_enrichment(ranked, max_fdr = 0.01)
  print(head(result[, c("category", "description", "direction", "enrichment_score", "fdr")]))

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
