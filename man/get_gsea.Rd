\name{get_gsea}
\alias{\S4method{get_gsea}{STRINGdb}}
\alias{get_gsea}
\title{
get_gsea
}
\description{
Runs STRING's values/ranks enrichment, a gene set enrichment analysis (GSEA) of a whole ranked experiment ("Proteins with Values/Ranks" on the STRING website).
The analysis runs on STRING's servers: the method submits a job to STRING's queue, waits for it and downloads the results.
}
\usage{
\S4method{get_gsea}{STRINGdb}(string_ids, values, max_fdr=0.01, rank_direction="descending", timeout=7200, api_key=Sys.getenv("STRINGDB_API_KEY"), quiet=FALSE, caller_identity="STRINGdb-package")
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
The identifiers and values are sent to STRING. The method checks the job's state every few seconds (from 5 s, growing to 30 s).
When the wait ends without results (the timeout, an interrupt with Ctrl-C, or a lost connection), the job keeps running on STRING, and the message gives its job identifier:
get its results later with \code{get_gsea_results(job_id)}. STRING keeps the results for a limited time; \code{get_gsea} downloads them as soon as the job has finished.
STRING tests each term at the top, at the bottom and at both ends of the ranked list (Kolmogorov-Smirnov and aggregate fold change tests); terms enriched in the centre of the list are not reported. The false discovery rate is computed within each category.
STRING keeps each job for the API key that submitted it, so \code{get_gsea_results} needs the same key.
}
\value{
A data frame with one row for each enriched term and the columns
\code{category}, \code{termID}, \code{termDescription}, \code{genesMapped} (proteins of the term in the input), \code{genesInSet} (proteins of the term in the genome),
\code{enrichmentScore}, \code{direction} (\code{"top"}, \code{"bottom"} or \code{"both ends"}), \code{falseDiscoveryRate}, \code{method},
\code{proteinIDs}, \code{proteinLabels}, \code{proteinInputLabels}, \code{proteinInputValues} and \code{proteinRanks} (comma-separated lists; STRING writes a comma inside an item, e.g. in an identifier of the input, as \code{&comma;}),
and the attributes \code{job_id}, \code{page_url} (the results on the STRING website), \code{graph_url} (STRING's enrichment figure), \code{download_url} and \code{string_version}.
}
\seealso{
\code{get_gsea_results}, \code{get_api_key}, \code{map}
}
\author{
Damian Szklarczyk
}
