\name{downloadAbsentFile}
\alias{downloadAbsentFile}
\title{
downloadAbsentFile
}
\description{
download a file only if it is not present.
}
\usage{
downloadAbsentFile(urlStr, oD = tempdir())
}
\arguments{
  \item{urlStr}{
url from which to download the file
}
\item{oD}{
directory where to store the file
}
}
\details{
The file is saved in \code{oD} under the last part of \code{urlStr}. It is downloaded only when that file is absent or empty; otherwise the file in \code{oD} is used.
The download is saved under a temporary name in \code{oD} and gets the file's name only when it has finished. A download that fails or is cut off part-way (for example by a timeout) stops with the error of \code{download.file} and leaves no file, so the next call downloads the whole file again.
While it downloads, R's option \code{timeout}, which limits the whole transfer (60 seconds unless set), is raised to at least 300 seconds, as the help of \code{download.file} recommends for large files. A higher value is kept, and the option is restored afterwards.
}
\value{
The path of the file, or \code{NULL}, with a message, when the download gave no file.
}
\author{
Andrea Franceschini
}
