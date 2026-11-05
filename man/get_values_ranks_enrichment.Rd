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
}
\usage{
\S4method{get_values_ranks_enrichment}{STRINGdb}(string_ids, values, max_fdr=0.01, rank_direction="descending", timeout=7200, api_key=Sys.getenv("STRINGDB_API_KEY"), quiet=FALSE, caller_identity="STRINGdb-package")
}
\arguments{
  \item{string_ids}{
a vector of STRING identifiers (e.g. the \code{STRING_id} column of \code{map}), one for each value.
Submit the whole experiment, e.g. all measured proteins, without any cutoff.
Rows without an identifier or without a finite value are left out, and an identifier given more than once keeps its value farthest from 0.
}
  \item{values}{
a numeric vector of the values to rank, e.g. log fold changes or t statistics.
}
  \item{max_fdr}{
the false discovery rate cutoff of the reported terms: above 0 and at most 0.25 (default 0.01).
}
  \item{rank_direction}{
\code{"descending"} (default: rank 1 is the largest value, and \code{"top"} is the end of the largest values) or \code{"ascending"} (rank 1 is the smallest value).
It only names the two ends of the ranked list and numbers the ranks; the enrichment itself is the same.
}
  \item{timeout}{
the number of seconds to wait for the job (default 7200).
}
  \item{api_key}{
a STRING API key (default: the environment variable \code{STRINGDB_API_KEY}); see \code{get_api_key}.
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

The identifiers and values are sent to STRING. The method checks the job's state every few seconds (from 5 s, growing to 30 s).
When the wait ends without results (the timeout, an interrupt with Ctrl-C, or a lost connection), the job keeps running on STRING, and the message gives its job identifier:
get its results later with \code{get_values_ranks_results(job_id)}. STRING keeps the results for a limited time; \code{get_values_ranks_enrichment} downloads them as soon as the job has finished.
STRING keeps each job for the API key that submitted it, so \code{get_values_ranks_results} needs the same key.
}
\value{
A data frame with one row for each enriched term and the columns
\code{category}, \code{termID}, \code{termDescription}, \code{genesMapped} (proteins of the term in the input), \code{genesInSet} (proteins of the term in the genome),
\code{enrichmentScore}, \code{direction} (\code{"top"}, \code{"bottom"} or \code{"both ends"}), \code{falseDiscoveryRate}, \code{method} (\code{"ks"} or \code{"afc"}),
\code{proteinIDs}, \code{proteinLabels}, \code{proteinInputLabels}, \code{proteinInputValues} and \code{proteinRanks} (comma-separated lists; STRING writes a comma inside an item, e.g. in an identifier of the input, as \code{&comma;}),
and the attributes \code{job_id}, \code{page_url} (the results on the STRING website), \code{graph_url} (STRING's enrichment figure), \code{download_url} and \code{string_version}.

\code{enrichmentScore} tells how far the values of the term's proteins lie towards their end of the list, from 0 to 10: 10 times the difference between their mean and the mean of all values, divided by the difference between the most extreme value at that end and the mean of all values.
For a term at both ends, the same is computed for the distances of the values from the mean of all values.
}
\seealso{
\code{get_values_ranks_results}, \code{get_api_key}, \code{get_enrichment}, \code{map}
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
