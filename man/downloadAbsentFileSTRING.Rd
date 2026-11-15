\name{downloadAbsentFileSTRING}
\alias{downloadAbsentFileSTRING}
\title{
downloadAbsentFileSTRING
}
\description{
download a STRING file only if it is not present or if it is corrupted.
It no longer works: the server it asks for the expected file size (string.uzh.ch) no longer exists. Use \code{downloadAbsentFile} instead.
}
\usage{
downloadAbsentFileSTRING(urlStr, oD = tempdir())
}
\arguments{
  \item{urlStr}{
url from which to download the file
}
\item{oD}{
directory where to store the file
}
}
\author{
Andrea Franceschini
}
