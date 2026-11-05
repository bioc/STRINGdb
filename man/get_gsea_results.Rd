\name{get_gsea_results}
\alias{\S4method{get_gsea_results}{STRINGdb}}
\alias{get_gsea_results}
\title{
get_gsea_results
}
\description{
Returns the results of a values/ranks job submitted with \code{get_gsea}, waiting for the job if it has not finished yet:
for example after the wait of \code{get_gsea} timed out or was interrupted, or in a new R session.
}
\usage{
\S4method{get_gsea_results}{STRINGdb}(job_id, timeout=7200, api_key=Sys.getenv("STRINGDB_API_KEY"), quiet=FALSE, caller_identity="STRINGdb-package")
}
\arguments{
  \item{job_id}{
the identifier of the job: the \code{job_id} attribute of the results of \code{get_gsea}, or the one in its messages.
}
  \item{timeout}{
the number of seconds to wait for the job (default 7200).
}
  \item{api_key}{
the STRING API key that submitted the job (default: the environment variable \code{STRINGDB_API_KEY}). STRING keeps each job for the key that submitted it.
}
  \item{quiet}{
\code{TRUE} to print no progress messages.
}
  \item{caller_identity}{
caller identifier sent to STRING.
}
}
\details{
STRING keeps the results of a job for a limited time; after that, submit the job again with \code{get_gsea}.
}
\value{
The results of the job, as returned by \code{get_gsea}.
}
\seealso{
\code{get_gsea}
}
\author{
Damian Szklarczyk
}
