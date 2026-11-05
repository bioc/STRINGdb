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
\S4method{get_values_ranks_enrichment}{STRINGdb}(string_ids, values, max_fdr=0.01, rank_direction="descending", timeout=7200, api_key=NULL, quiet=FALSE, caller_identity="STRINGdb-package")
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers (e.g. the \code{STRING_id} column of \code{map}), one for each value.
Submit the whole experiment, e.g. all measured proteins, without any cutoff.
Rows without an identifier or without a finite value are left out, and an identifier given more than once keeps its value farthest from 0.
}
  \item{values}{
the values to rank, e.g. log fold changes or t statistics: a numeric vector, or a matrix or data frame with one numeric column for each experiment (named by its column) and one row for each identifier of \code{string_ids}.
In a table, a missing value (\code{NA}) leaves the protein out of that experiment only.
}
  \item{max_fdr}{
the false discovery rate cutoff of the reported terms: above 0 and at most 0.25 (default 0.01).
}
  \item{rank_direction}{
\code{"descending"} (default: rank 1 is the largest value, and \code{"top"} is the end of the largest values) or \code{"ascending"} (rank 1 is the smallest value).
It only names the two ends of the ranked list and numbers the ranks; the enrichment itself is the same.
}
  \item{timeout}{
the number of seconds to wait for the jobs (default 7200).
}
  \item{api_key}{
a STRING API key. The default, \code{NULL}, uses the key of this computer: the environment variable \code{STRINGDB_API_KEY} when it is set, otherwise the key that the package saved, otherwise a new key, which the package requests from STRING and saves (see \code{get_api_key}).
}
  \item{quiet}{
\code{TRUE} to print no progress messages.
}
  \item{caller_identity}{
caller identifier sent to STRING.
}
}
\details{
The values/ranks enrichment is a GSEA in purpose: like \code{get_enrichment} it tests the terms of STRING's categories, but against the whole ranked list instead of a selected set of proteins.
Its statistics differ from the GSEA of Subramanian et al. (2005), as implemented for example in the fgsea and clusterProfiler packages, which computes a weighted running-sum enrichment score, normalizes it (NES) and assesses it by permutations.
STRING tests each term with a Kolmogorov-Smirnov test of the ranks of its proteins, and a small term with a borderline result also with a permutation-based aggregate fold change test (column \code{method}: \code{"ks"} or \code{"afc"}).
A term can be enriched at the top, at the bottom or at both ends of the list; terms enriched in the centre of the list are not reported.
The false discovery rate is the Benjamini-Hochberg FDR within each category, and \code{enrichmentScore} is STRING's effect strength (see Value), not the enrichment score of that GSEA: the scores and FDRs of the two methods cannot be compared.

The identifiers and values are sent to STRING. The method checks the state of the jobs every few seconds (from 5 s, growing to 30 s) and downloads the results of each job as soon as it has finished, as STRING keeps them only for a limited time; a download that fails is tried again.
When the wait ends early, the jobs keep running on STRING; get their results later with \code{get_values_ranks_results}.
For a vector of values, the timeout, an interrupt with Ctrl-C or a lost connection ends the wait with an error whose message gives the job identifier.
For a table of values, the timeout returns the results so far (see Value), and an interrupt or a lost connection ends with an error whose message lists the job identifiers.
STRING keeps each job for the API key that submitted it, so \code{get_values_ranks_results} needs the same key; the default \code{api_key=NULL} gives the same key on the same computer.

\strong{Many experiments at once.} With a table of values, the method submits one job for each experiment before it waits, so that STRING's queue runs them side by side on STRING's servers, and returns all results.
Up to 1000 jobs of an API key may wait or run on STRING at once: when STRING refuses a job because of this limit, the method submits it as soon as other jobs have finished.
A job that fails does not stop the others. To submit without waiting, e.g. many experiments whose results are collected later or in another R session, use \code{submit_values_ranks_enrichment}, \code{get_values_ranks_status} and \code{get_values_ranks_results}.
}
\value{
For a vector of values, a data frame with one row for each enriched term and the columns
\code{category}, \code{termID}, \code{termDescription}, \code{genesMapped} (proteins of the term in the input), \code{genesInSet} (proteins of the term in the genome),
\code{enrichmentScore}, \code{direction} (\code{"top"}, \code{"bottom"} or \code{"both ends"}), \code{falseDiscoveryRate}, \code{method} (\code{"ks"} or \code{"afc"}),
\code{proteinIDs}, \code{proteinLabels}, \code{proteinInputLabels}, \code{proteinInputValues} and \code{proteinRanks} (comma-separated lists; STRING writes a comma inside an item, e.g. in an identifier of the input, as \code{&comma;}),
and the attributes \code{job_id}, \code{page_url} (the results on the STRING website), \code{graph_url} (STRING's enrichment figure), \code{download_url} and \code{string_version}.

For a table of values, a list of such data frames named by experiment, and the job identifiers, named by experiment, as the attribute \code{job_ids}.
An experiment has \code{NULL} when its job failed (a warning gives STRING's reason), when the timeout ended the wait before its job finished, or when it was not submitted (the timeout came first, the submissions to STRING kept failing, or STRING refused the API key): warnings then say so.
\code{get_values_ranks_results(attr(results, "job_ids"))} returns the results of all submitted jobs later, and the column numbers of the experiments that were not submitted, named by experiment, are the attribute \code{not_submitted}, so that \code{values[, attr(results, "not_submitted"), drop = FALSE]} are their values.

\code{enrichmentScore} tells how far the values of the term's proteins lie towards their end of the list, from 0 to 10: 10 times the difference between their mean and the mean of all values, divided by the difference between the most extreme value at that end and the mean of all values.
For a term at both ends, the same is computed for the distances of the values from the mean of all values.
}
\seealso{
\code{submit_values_ranks_enrichment}, \code{get_values_ranks_status}, \code{get_values_ranks_results}, \code{get_api_key}, \code{get_enrichment}, \code{map}
}
\references{
Subramanian A, et al. (2005). Gene set enrichment analysis: a knowledge-based approach for interpreting genome-wide expression profiles. Proc Natl Acad Sci USA 102(43):15545-15550.
}
\concept{GSEA}
\concept{gene set enrichment analysis}
\concept{ranked list enrichment}
\author{
Damian Szklarczyk
}
