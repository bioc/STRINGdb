# Offline tests of downloadAbsentFile(), which keeps the downloaded STRING files in the download cache
# (input_directory). Nothing is downloaded from STRING: a whole download is a file:// address of a file of
# this test, and a download that is cut off part-way is download.file() traced to do what it does then:
# it leaves the part that arrived in its file and stops with an error.

# A file:// address of a local file, also on Windows (file:///C:/...).
local_file_url <- function(path) {
  path <- normalizePath(path, winslash = "/")
  if (substr(path, 1, 1) != "/") {
    path <- paste("/", path, sep = "")
  }
  return(paste("file://", path, sep = ""))
}


test_download_absent_file <- function() {

  cache <- tempfile("download_cache")
  source_directory <- tempfile("download_source")
  dir.create(cache)
  dir.create(source_directory)
  on.exit(unlink(c(cache, source_directory), recursive = TRUE), add = TRUE)

  info_name <- "9606.protein.info.v12.5.txt.gz"
  info_source <- file.path(source_directory, info_name)
  writeLines(c("the", "whole", "file"), info_source)

  # A whole download is saved under the file's name, and no temporary file is left.
  path <- STRINGdb::downloadAbsentFile(local_file_url(info_source), oD = cache)
  checkEquals(paste(cache, "/", info_name, sep = ""), path)
  checkEquals(c("the", "whole", "file"), readLines(path))
  checkEquals(info_name, list.files(cache))

  # A file in the cache is not downloaded again.
  writeLines("a newer file", info_source)
  path <- STRINGdb::downloadAbsentFile(local_file_url(info_source), oD = cache)
  checkEquals(c("the", "whole", "file"), readLines(path))

  # An empty file in the cache is downloaded again.
  writeLines(character(0), path)
  path <- STRINGdb::downloadAbsentFile(local_file_url(info_source), oD = cache)
  checkEquals("a newer file", readLines(path))
  checkEquals(info_name, list.files(cache))

  # A download that is cut off part-way stops with the error of download.file() and leaves nothing in the
  # cache, neither the part that arrived nor its temporary file, so that the next call downloads the whole
  # file.
  aliases_name <- "9606.protein.aliases.v12.5.txt.gz"
  aliases_source <- file.path(source_directory, aliases_name)
  writeLines(c("all", "aliases"), aliases_source)

  # The trace is removed also when the test stops early: the later tests of the same R session download
  # with download.file() as well.
  untrace_download <- function() {
    suppressMessages(untrace("download.file", where = asNamespace("STRINGdb")))
  }
  suppressMessages(trace("download.file", where = asNamespace("STRINGdb"), print = FALSE,
    tracer = quote({
      writeLines("the part that arrived", destfile)
      warning("URL '", url, "': status was 'Transferred a partial file'")
      stop("download from '", url, "' failed")
    })))
  download_traced <- TRUE
  on.exit(if (download_traced) untrace_download(), add = TRUE)

  error_message <- tryCatch(
    {
      suppressWarnings(STRINGdb::downloadAbsentFile(local_file_url(aliases_source), oD = cache))
      ""
    },
    error = function(e) conditionMessage(e)
  )
  untrace_download()
  download_traced <- FALSE

  checkTrue(grepl("^download from 'file://.*failed$", error_message))
  checkEquals(info_name, list.files(cache))

  path <- STRINGdb::downloadAbsentFile(local_file_url(aliases_source), oD = cache)
  checkEquals(c("all", "aliases"), readLines(path))
  checkEquals(sort(c(info_name, aliases_name)), sort(list.files(cache)))

}
