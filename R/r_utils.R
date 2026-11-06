#
#  copyright:   Andrea Franceschini
#          (Swiss Institute of Bioinformatics)
#           andrea.franceschini@isb-sib.ch
#


# Stops when the STRING server (or the proxy in front of it) failed to answer,
# for example with a 524 gateway timeout. The body of such a response is an
# HTML or JSON error page, which the callers would otherwise parse as if it
# were the requested table or image. Client errors (4xx), such as the 404 that
# STRING returns for unknown identifiers, are deliberately not handled here
# to keep the established behaviour of the public methods; only the methods
# that return pictures stop on them too (stop_on_client_error).
stop_on_server_error <- function(res, uri) {
  status <- status_code(res)
  if (status >= 500) {
    stop(
      "ERROR: STRING request failed with HTTP status ", status, " for ", uri,
      ". The STRING server did not return a valid answer; please try again later.",
      call. = FALSE
    )
  }
  invisible(res)
}


# Decodes the HTML entities that STRING writes in its error messages. "&amp;" is decoded
# last, so that a written "&amp;lt;" becomes "&lt;" and not "<".
decode_html_entities <- function(text) {
  text <- gsub("&quot;", '"', text, fixed = TRUE)
  text <- gsub("&#x27;", "'", text, fixed = TRUE)
  text <- gsub("&#39;", "'", text, fixed = TRUE)
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  text <- gsub("&gt;", ">", text, fixed = TRUE)
  text <- gsub("&nbsp;", " ", text, fixed = TRUE)
  text <- gsub("&amp;", "&", text, fixed = TRUE)
  return(text)
}


# Reads STRING's explanation from the body of an API error answer. The image,
# highres_image and svg methods answer a request that they refuse with a small
# JSON object, { "Error" : "<title>", "ErrorMessage" : "<message>" }, in which
# double quotes are written as &quot; and the message may contain HTML tags.
# Returns "<title>: <message>" as plain text (the message alone when the title
# is only "Error"), or NULL when the body has neither.
string_api_error_message <- function(body_text) {
  if (is.null(body_text) || length(body_text) != 1 || is.na(body_text)) {
    return(NULL)
  }

  read_field <- function(field_name) {
    pattern <- paste('"', field_name, '"[[:space:]]*:[[:space:]]*"([^"]*)"', sep = "")
    field_match <- regmatches(body_text, regexec(pattern, body_text))[[1]]
    if (length(field_match) < 2) {
      return(NULL)
    }

    text <- field_match[2]
    text <- decode_html_entities(gsub("<[^>]*>", " ", text))
    text <- trimws(gsub("[[:space:]]+", " ", text))

    if (nchar(text) == 0) {
      return(NULL)
    }
    return(text)
  }

  title <- read_field("Error")
  message <- read_field("ErrorMessage")

  if (is.null(title) && is.null(message)) {
    return(NULL)
  }
  if (is.null(message)) {
    return(title)
  }
  if (is.null(title) || tolower(title) == "error") {
    return(message)
  }
  return(paste(title, ": ", message, sep = ""))
}


# Stops when STRING refused the request (HTTP status 4xx), with STRING's own
# explanation: for example a PNG of a network with more interactions than STRING
# draws as a PNG (150,000), for which it suggests the SVG or a higher
# required_score. The methods that return pictures (get_png,
# get_enrichment_figure) use it; they would otherwise read STRING's error answer
# as a PNG, or return it as if it were the SVG.
stop_on_client_error <- function(res, uri) {
  status <- status_code(res)
  if (status >= 400 && status < 500) {
    body_text <- NULL
    if (is.raw(res$content)) {
      body_text <- tryCatch(rawToChar(res$content), error = function(e) NULL)
      if (!is.null(body_text)) {
        Encoding(body_text) <- "UTF-8"
      }
    }

    explanation <- string_api_error_message(body_text)
    if (is.null(explanation)) {
      explanation <- "STRING did not say why."
    }

    stop(
      "ERROR: STRING refused the request with HTTP status ", status, " for ", uri, ": ", explanation,
      call. = FALSE
    )
  }
  invisible(res)
}


# The caller identity of the package. The request helpers (postFormSmart, post_string_json and
# get_string_text, and the requests of fetch_string_version_table and get_enrichment_figure) add it
# to every request to STRING's API, and downloadAbsentFile to the downloads of STRING's data files,
# so that no method handles it. STRING logs it, and makes an API key requested by STRINGdb work at
# once (other new keys work after a few minutes).
PACKAGE_CALLER_IDENTITY <- "STRINGdb-package"


# The parameters of a request to STRING, with the package's caller identity.
with_caller_identity <- function(params) {
  params <- as.list(params)
  params$caller_identity <- PACKAGE_CALLER_IDENTITY
  return(params)
}

# Whether the warning about an ignored caller_identity argument was given in this R session.
caller_identity_warning <- new.env(parent = emptyenv())


# get_png, get_link, get_enrichment_figure and get_geneset_description keep their caller_identity
# argument (default NULL) until the next Bioconductor release, so that older code runs, but the
# package sends its own caller identity. A given caller_identity gets a warning, once per R
# session, so that a loop does not repeat it.
warn_ignored_caller_identity <- function(caller_identity) {
  if (is.null(caller_identity) || isTRUE(caller_identity_warning$given)) {
    return(invisible(NULL))
  }
  caller_identity_warning$given <- TRUE
  warning(
    "The argument caller_identity is ignored: STRINGdb sends its own caller identity to STRING. ",
    "It will be removed in the next Bioconductor release, so do not specify it.",
    call. = FALSE
  )
  return(invisible(NULL))
}


# The address of a download from STRING's servers (string-db.org, stringdb-downloads.org), with the
# package's caller identity after the parameters already in it. Other addresses are returned as
# they are: downloadAbsentFile is exported, and another server may refuse an unknown parameter.
url_with_caller_identity <- function(url_str) {
  string_server <- "^https?://([^/]+\\.)?(string-db\\.org|stringdb-downloads\\.org)(:[0-9]+)?(/|$)"
  if (!grepl(string_server, url_str)) {
    return(url_str)
  }
  separator <- if (grepl("?", url_str, fixed = TRUE)) "&" else "?"
  return(paste(url_str, separator, "caller_identity=", PACKAGE_CALLER_IDENTITY, sep = ""))
}


# .stop_on_client_error: also stop when STRING refused the request (HTTP status
# 4xx), with STRING's explanation (see stop_on_client_error)
postFormSmart <- function(uri, .params = list(), .opts = list(), .ctype = "text", .stop_on_client_error = FALSE) {
  res <- POST(url = uri, body = with_caller_identity(.params), config = .opts)
  stop_on_server_error(res, uri)
  if (.stop_on_client_error) {
    stop_on_client_error(res, uri)
  }
  data <- content(res, as = .ctype)
  return(data)
}


coeffOfvar <- function(x) {
  return(sd(x) / mean(x))
}


PREFERRED_ALIAS_SOURCES <- c(
  "BioMart_HUGO",
  "Ensembl_HGNC",
  "SGD_NAME",
  "FlyBase_PRIMARY",
  "TAIR_PRIMARY",
  "SGD_PRIMARY",
  "PomBase_NAME",
  "Ensembl_SGD",
  "Ensembl_TAIR",
  "Ensembl_MGI",
  "Ensembl_FlyBaseName_gene",
  "Ensembl_flybase_gene_id",
  "Ensembl_EntrezGene",
  "Ensembl_WikiGene",
  "Ensembl_HGNC_automatic_gene",
  "Ensembl_MGI_automatic_gene",
  "Ensembl_HGNC_curated_gene",
  "RefSeq_gene",
  "PomBase_ID",
  "Ensembl_UniProt_GN",
  "UniProt_GN_Name",
  "KEGG_NAME",
  "MaizeGDB_SYMBOL",
  "WormBase_PRIMARY",
  "FlyBase_SYNONYM",
  "TAIR_SYNONYM",
  "RefSeq",
  "UniProt_GN_ORFNames",
  "UniProt_ID"
)


# Normalizes identifiers and aliases for the case-insensitive mapping: the text
# is read as Windows-1252 and upper-cased, as before. Bytes that Windows-1252
# does not define (0x81, 0x8D, 0x8F, 0x90 and 0x9D, which occur in UTF-8
# characters such as the hyphen U+2010 of "spine-associated RapGAP 3") are kept
# as byte codes ("<90>"); iconv() otherwise turns the whole text into NA, and
# merge() then matched those aliases to every NA identifier. NA stays NA.
normalize_alias_key <- function(x) {
  return(toupper(iconv(x, "WINDOWS-1252", "UTF-8", sub = "byte")))
}


prioritize_aliases_by_source <- function(aliasDf) {
  # an NA alias (read.table reads the alias "NA" as NA) can never match an
  # identifier; left out, it cannot match the NA identifiers of match() or
  # merge() either, in map() or in code that uses get_aliases()
  aliasDf <- aliasDf[!is.na(aliasDf$alias), , drop = FALSE]

  if (nrow(aliasDf) == 0) {
    return(aliasDf)
  }

  rank_lookup <- seq_along(PREFERRED_ALIAS_SOURCES)
  names(rank_lookup) <- PREFERRED_ALIAS_SOURCES

  aliasDf$alias_normalized <- normalize_alias_key(aliasDf$alias)
  ambiguous <- duplicated(aliasDf$alias_normalized) | duplicated(aliasDf$alias_normalized, fromLast = TRUE)

  if (!any(ambiguous)) {
    return(subset(aliasDf, select = c("STRING_id", "alias")))
  }

  aliasDf$row_order <- seq_len(nrow(aliasDf))
  aliasDf$first_alias_order <- match(aliasDf$alias_normalized, aliasDf$alias_normalized)

  aliasDf_ambiguous <- aliasDf[ambiguous, ]
  aliasDf_ambiguous$source_rank <- length(PREFERRED_ALIAS_SOURCES) + 100
  aliasDf_ambiguous$source_rank[aliasDf_ambiguous$sources == "preferred_name"] <- 0
  aliasDf_ambiguous$source_rank[aliasDf_ambiguous$sources == "STRING_id"] <- 1

  ranked_sources <- rank_lookup[aliasDf_ambiguous$sources]
  aliasDf_ambiguous$source_rank[!is.na(ranked_sources)] <- unname(ranked_sources[!is.na(ranked_sources)]) + 1

  aliasDf_ambiguous <- aliasDf_ambiguous[order(aliasDf_ambiguous$alias_normalized, aliasDf_ambiguous$source_rank, aliasDf_ambiguous$row_order), ]
  aliasDf_ambiguous <- aliasDf_ambiguous[!duplicated(aliasDf_ambiguous$alias_normalized), ]
  aliasDf_ambiguous$source_rank <- NULL

  aliasDf_unambiguous <- aliasDf[!ambiguous, ]
  aliasDf_selected <- rbind(aliasDf_unambiguous, aliasDf_ambiguous)
  aliasDf_selected <- aliasDf_selected[order(aliasDf_selected$first_alias_order), ]

  return(subset(aliasDf_selected, select = c("STRING_id", "alias")))
}


format_interaction_dataframe <- function(interactions_df, proteins_df, from_col = "from", to_col = "to") {
  interactions_df$from_name <- proteins_df$preferred_name[match(interactions_df[, from_col], proteins_df$protein_external_id)]
  interactions_df$to_name <- proteins_df$preferred_name[match(interactions_df[, to_col], proteins_df$protein_external_id)]

  leading_cols <- c(from_col, to_col, "combined_score", "from_name", "to_name")
  trailing_cols <- setdiff(names(interactions_df), leading_cols)
  return(interactions_df[, c(leading_cols, trailing_cols), drop = FALSE])
}


collapse_string_identifiers <- function(string_ids) {
  if (is.null(string_ids) || length(string_ids) == 0) {
    return(NULL)
  }

  string_ids <- unique(string_ids)
  string_ids <- string_ids[!is.na(string_ids)]

  if (length(string_ids) == 0) {
    return(NULL)
  }

  return(paste(string_ids, collapse = "%0d"))
}


drop_null_params <- function(params) {
  return(params[!vapply(params, is.null, logical(1))])
}


normalize_api_flag <- function(value, param_name) {
  if (is.null(value)) {
    return(NULL)
  }

  if (is.logical(value) && length(value) == 1 && !is.na(value)) {
    return(as.integer(value))
  }

  if (is.numeric(value) && length(value) == 1 && !is.na(value) && value %in% c(0, 1)) {
    return(as.integer(value))
  }

  stop(paste("ERROR:", param_name, "should be TRUE/FALSE or 0/1.", sep = " "))
}


# Checks a number of proteins for STRING's API, such as add_color_nodes and
# add_white_nodes of the network pictures and links (the number of interaction
# partners that STRING adds to the network): a whole number of at least 0,
# returned as an integer. NULL stays NULL, so that nothing is sent.
normalize_api_count <- function(value, param_name) {
  if (is.null(value)) {
    return(NULL)
  }

  is_whole_number <- is.numeric(value) && length(value) == 1 && !is.na(value) && is.finite(value) &&
    value == round(value)

  if (is_whole_number && value >= 0 && value <= .Machine$integer.max) {
    return(as.integer(value))
  }

  stop("ERROR: ", param_name, " should be a whole number of at least 0.", call. = FALSE)
}


normalize_network_type <- function(value) {
  if (length(value) != 1 || is.na(value)) {
    return(NULL)
  }

  value <- tolower(value)
  if (value == "functional") {
    value <- "full"
  }

  if (!(value %in% c("full", "physical", "regulatory"))) {
    return(NULL)
  }

  return(value)
}


network_type_to_api <- function(network_type) {
  network_type <- normalize_network_type(network_type)
  if (is.null(network_type)) {
    stop("ERROR: invalid network type.")
  }

  if (network_type == "full") {
    return("functional")
  }

  return(network_type)
}


network_type_to_download_prefix <- function(network_type) {
  network_type <- normalize_network_type(network_type)
  if (is.null(network_type)) {
    stop("ERROR: invalid network type.")
  }

  if (network_type == "full") {
    return("")
  }

  return(paste(network_type, ".", sep = ""))
}


is_regulatory_network <- function(network_type) {
  return(identical(normalize_network_type(network_type), "regulatory"))
}


is_regulatory_network_version_supported <- function(version) {
  version_number <- suppressWarnings(as.numeric(version))
  return(!is.na(version_number) && version_number >= 12.5)
}


# The STRING versions known to this release of the package, oldest first, with
# their stable (version-specific) addresses as STRING's API method
# available_api_versions lists them. The constructor takes a known version's
# address from this table instead of asking string-db.org, so that a STRINGdb
# object for a known version is created without any request to STRING: offline,
# and when string-db.org cannot be reached (the Bioconductor build machines
# failed with SSL errors on these requests). A version missing from the table,
# such as one released after this package, is still looked up on
# string-db.org. Add each new STRING version as the last row.
KNOWN_STRING_VERSIONS <- data.frame(
  version = c("11.0", "11.0b", "11.5", "12.0", "12.5"),
  stable_url = c(
    "https://version-11-0.string-db.org",
    "https://version-11-0b.string-db.org",
    "https://version-11-5.string-db.org",
    "https://version-12-0.string-db.org",
    "https://version-12-5.string-db.org"
  ),
  stringsAsFactors = FALSE
)


# Seconds that the constructor waits for string-db.org when it has to ask for
# the current STRING version, or for a version that the package does not know.
STRING_VERSION_REQUEST_TIMEOUT <- 5


# Reads a version table of the STRING API in the tsv-no-header format: one line
# per version, the version and its stable address separated by a tab, e.g.
# "12.5<TAB>https://version-12-5.string-db.org". Returns a data frame with the
# columns version and stable_url, or NULL when the text is not such a table
# (for example an HTML error page).
parse_string_version_table <- function(body_text) {
  if (is.null(body_text) || length(body_text) != 1 || is.na(body_text)) {
    return(NULL)
  }

  lines <- strsplit(body_text, "\r?\n")[[1]]
  lines <- lines[nchar(trimws(lines)) > 0]
  if (length(lines) == 0) {
    return(NULL)
  }

  fields <- strsplit(lines, "\t", fixed = TRUE)
  if (any(lengths(fields) < 2)) {
    return(NULL)
  }

  versions <- trimws(vapply(fields, function(line_fields) line_fields[1], character(1)))
  stable_urls <- trimws(vapply(fields, function(line_fields) line_fields[2], character(1)))

  looks_like_versions <- all(grepl("^[0-9]+\\.[0-9]+[a-z]?$", versions))
  looks_like_addresses <- all(grepl("^https?://", stable_urls))
  if (!looks_like_versions || !looks_like_addresses) {
    return(NULL)
  }

  return(data.frame(version = versions, stable_url = stable_urls, stringsAsFactors = FALSE))
}


# Asks string-db.org for one of its version tables: "version" (the current
# STRING version) or "available_api_versions" (all versions that the API
# serves). Returns a data frame with the columns version and stable_url, or
# NULL when STRING cannot be reached within timeout_seconds or does not answer
# with such a table: an SSL or connection error, a timeout, an HTTP error
# status, or an error page.
fetch_string_version_table <- function(method, protocol = "https",
                                       timeout_seconds = STRING_VERSION_REQUEST_TIMEOUT) {
  url_str <- paste(protocol, "://string-db.org/api/tsv-no-header/", method, sep = "")

  response <- tryCatch(
    GET(url_str, config = timeout(timeout_seconds), query = with_caller_identity(list())),
    error = function(e) NULL
  )
  if (is.null(response) || status_code(response) != 200) {
    return(NULL)
  }

  body_text <- tryCatch(
    content(response, as = "text", encoding = "UTF-8"),
    error = function(e) NULL
  )
  return(parse_string_version_table(body_text))
}


# Decides the STRING version of a STRINGdb object and its stable address (the
# constructor changes the address to http when protocol is "http"). Returns a
# list with the elements version and stable_url.
#   version              the version asked for; character(0) when none was
#                        given. A version without a dot gets ".0" ("11"
#                        becomes "11.0"), as before.
#   protocol             "https" or "http", for the requests to string-db.org
#   fetch_version_table  the function that asks string-db.org for a version
#                        table (fetch_string_version_table); an argument so
#                        that the unit tests can replace the request
# A known version (KNOWN_STRING_VERSIONS) needs no request. Without a version,
# string-db.org is asked for its current version, as before; when STRING cannot
# be reached, the newest known version is used and a WARNING is printed. A
# version that the package does not know is looked up among the versions that
# string-db.org serves, and the function stops when it is not among them or
# when STRING cannot be reached to check it.
resolve_string_version <- function(version, protocol = "https",
                                   fetch_version_table = fetch_string_version_table) {
  known_versions <- KNOWN_STRING_VERSIONS

  if (length(version) == 0) {
    current_version_table <- fetch_version_table("version", protocol)

    if (is.null(current_version_table)) {
      newest_known_version <- known_versions$version[nrow(known_versions)]
      cat(
        "WARNING: You didn't specify a version of the STRING database to use, and STRING could not be reached ",
        "to look up its current version. Hence we will use STRING ", newest_known_version,
        ", the newest version known to this package.\n",
        sep = ""
      )
      return(list(
        version = newest_known_version,
        stable_url = known_versions$stable_url[nrow(known_versions)]
      ))
    }

    current_version <- current_version_table$version[1]
    cat("WARNING: You didn't specify a version of the STRING database to use. Hence we will use STRING ", current_version, "\n")

    # a current version released after this package takes its address from STRING's answer
    if (current_version %in% known_versions$version) {
      current_stable_url <- known_versions$stable_url[known_versions$version == current_version]
    } else {
      current_stable_url <- current_version_table$stable_url[1]
    }
    return(list(version = current_version, stable_url = current_stable_url))
  }

  if (!(grepl("\\.", version))) {
    version <- paste(version, ".0", sep = "")
  }

  if (version %in% known_versions$version) {
    return(list(
      version = version,
      stable_url = known_versions$stable_url[known_versions$version == version]
    ))
  }

  available_versions_table <- fetch_version_table("available_api_versions", protocol)

  if (is.null(available_versions_table)) {
    stop(
      "ERROR: STRING version ", version, " is not known to this version of the STRINGdb package, and ",
      "string-db.org could not be reached to check whether it is available. Versions known to the package: ",
      paste(known_versions$version, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (!(version %in% available_versions_table$version)) {
    stop(
      "ERROR: specified version ", version, " is not valid. Available versions: ",
      paste(available_versions_table$version, collapse = ", "), ".",
      call. = FALSE
    )
  }

  return(list(
    version = version,
    stable_url = available_versions_table$stable_url[available_versions_table$version == version][1]
  ))
}


normalize_network_flavor <- function(value, network_type) {
  if (length(value) != 1 || is.na(value)) {
    stop("ERROR: network_flavor should be one of: 'evidence', 'confidence' or 'typed'.")
  }

  value <- tolower(value)
  if (value == "actions") {
    warning("network_flavor='actions' is a compatibility alias; using 'typed'.")
    value <- "typed"
  }

  if (!(value %in% c("evidence", "confidence", "typed"))) {
    stop("ERROR: network_flavor should be one of: 'evidence', 'confidence' or 'typed'.")
  }

  normalized_network_type <- normalize_network_type(network_type)
  if (is.null(normalized_network_type)) {
    stop("ERROR: invalid network type.")
  }

  if (value == "typed" && normalized_network_type != "full") {
    stop("ERROR: network_flavor='typed' is available only with network_type='full' (or 'functional').")
  }

  return(value)
}


normalize_typed_network_controls <- function(typed_physical_edges,
                                             typed_regulatory_edges,
                                             show_regulatory_signs,
                                             network_flavor) {
  typed_physical_edges <- normalize_api_flag(typed_physical_edges, "typed_physical_edges")
  typed_regulatory_edges <- normalize_api_flag(typed_regulatory_edges, "typed_regulatory_edges")
  show_regulatory_signs <- normalize_api_flag(show_regulatory_signs, "show_regulatory_signs")

  typed_controls <- c(typed_physical_edges, typed_regulatory_edges, show_regulatory_signs)
  if (length(typed_controls) > 0 && network_flavor != "typed") {
    stop("ERROR: typed edge controls are available only with network_flavor='typed'.")
  }

  if (identical(typed_regulatory_edges, 0L) && identical(show_regulatory_signs, 1L)) {
    stop("ERROR: show_regulatory_signs requires typed_regulatory_edges=TRUE.")
  }

  return(list(
    typed_physical_edges = typed_physical_edges,
    typed_regulatory_edges = typed_regulatory_edges,
    show_regulatory_signs = show_regulatory_signs
  ))
}


# Checks the optional filters of STRING's enrichment results (the enrichment
# table and the enrichment figure) and returns them as API parameters: the
# filters given, as text, and NULL for the others. STRING applies them like the
# filters of the enrichment table on its network page; without them, nothing
# is filtered.
#   max_fdr           highest FDR of a term (above 0, at most 1)
#   min_gene_count    lowest number of input proteins annotated with a term (at least 1)
#   min_strength      lowest strength of a term, log10(observed / expected)
#   min_signal        lowest signal of a term, STRING's balance of strength and FDR
#   merge_similarity  terms whose protein sets overlap at least this much (above 0,
#                     at most 1; 1 merges identical sets only) are merged into the
#                     most significant of them
normalize_enrichment_filters <- function(max_fdr = NULL,
                                         min_gene_count = NULL,
                                         min_strength = NULL,
                                         min_signal = NULL,
                                         merge_similarity = NULL) {
  is_single_number <- function(value) {
    return(is.numeric(value) && length(value) == 1 && !is.na(value) && is.finite(value))
  }

  if (!is.null(max_fdr)) {
    if (!(is_single_number(max_fdr) && max_fdr > 0 && max_fdr <= 1)) {
      stop("ERROR: max_fdr should be a number above 0 and at most 1.")
    }
    max_fdr <- as.character(max_fdr)
  }

  if (!is.null(min_gene_count)) {
    if (!(is_single_number(min_gene_count) && min_gene_count >= 1 && min_gene_count == round(min_gene_count))) {
      stop("ERROR: min_gene_count should be a whole number of at least 1.")
    }
    min_gene_count <- as.character(as.integer(min_gene_count))
  }

  if (!is.null(min_strength)) {
    if (!is_single_number(min_strength)) {
      stop("ERROR: min_strength should be a number.")
    }
    min_strength <- as.character(min_strength)
  }

  if (!is.null(min_signal)) {
    if (!is_single_number(min_signal)) {
      stop("ERROR: min_signal should be a number.")
    }
    min_signal <- as.character(min_signal)
  }

  if (!is.null(merge_similarity)) {
    if (!(is_single_number(merge_similarity) && merge_similarity > 0 && merge_similarity <= 1)) {
      stop("ERROR: merge_similarity should be a number above 0 and at most 1.")
    }
    merge_similarity <- as.character(merge_similarity)
  }

  return(list(
    max_fdr = max_fdr,
    min_gene_count = min_gene_count,
    min_strength = min_strength,
    min_signal = min_signal,
    merge_similarity = merge_similarity
  ))
}


# Checks get_enrichment's add_scores (TRUE/FALSE or 1/0) and returns the API
# parameter to send for it. With add_scores=1, STRING adds the strength and
# signal columns to the enrichment table, before p_value. FALSE is STRING's
# default and is not sent, so that a call without the option asks what it
# asked before. STRING 12.0 and 11.x ignore add_scores without a word, so it
# is sent only to version 12.5 or later (options_supported_by_version); an
# older version gets nothing, with a warning.
enrichment_score_params <- function(add_scores, version) {
  add_scores <- normalize_api_flag(add_scores, "add_scores")

  score_params <- list()
  if (identical(add_scores, 1L)) {
    score_params$add_scores <- add_scores
  }

  score_params <- options_supported_by_version(
    score_params, version, "The strength and signal columns (add_scores)"
  )
  return(score_params)
}


# Checks the clustering options of a network picture and returns them as STRING
# API parameters (an empty list without an algorithm). STRING clusters the
# picture's network as the Clusters tab of its network page does, and colours
# the proteins by cluster:
#   algorithm   "kmeans", "mcl" or "leiden" (in any case; STRING's names are
#               'kmeans', 'MCL' and 'leiden')
#   parameter   k-means: the number of clusters, a whole number of at least 1
#               (default 3); MCL: the inflation, from 1.1 to 10 (default 3);
#               Leiden: the resolution, from 0.1 to 10 (default 1). The ranges
#               are those of the Clusters tab.
#   cluster_aware_layout  lays the network out by its clusters (TRUE/FALSE or 0/1)
#   edge_visibility       the edges between clusters: "faded" (STRING's default),
#                         "dotted", "solid" or "noshow"
#   show_cluster_names    writes the names of the clusters on the picture (TRUE/FALSE or 0/1)
#   cluster_name_size     the size of those names: 1, 1.5, 2, 2.5, 3, 3.5, 4, 5 or 6
#                         (default: the size that suits the picture)
# The other options need an algorithm.
normalize_network_clustering <- function(algorithm = NULL,
                                         parameter = NULL,
                                         cluster_aware_layout = NULL,
                                         edge_visibility = NULL,
                                         show_cluster_names = NULL,
                                         cluster_name_size = NULL) {
  cluster_aware_layout <- normalize_api_flag(cluster_aware_layout, "cluster_aware_layout")
  show_cluster_names <- normalize_api_flag(show_cluster_names, "show_cluster_names")

  if (is.null(algorithm)) {
    other_options <- list(parameter, cluster_aware_layout, edge_visibility, show_cluster_names, cluster_name_size)
    if (any(!vapply(other_options, is.null, logical(1)))) {
      stop("ERROR: the clustering options need network_clustering_algorithm ('kmeans', 'mcl' or 'leiden').")
    }
    return(list())
  }

  if (!(is.character(algorithm) && length(algorithm) == 1 && !is.na(algorithm) &&
    tolower(algorithm) %in% c("kmeans", "mcl", "leiden"))) {
    stop("ERROR: network_clustering_algorithm should be one of: 'kmeans', 'mcl' or 'leiden'.")
  }
  algorithm <- tolower(algorithm)

  is_single_number <- function(value) {
    return(is.numeric(value) && length(value) == 1 && !is.na(value) && is.finite(value))
  }

  if (algorithm == "kmeans") {
    api_algorithm <- "kmeans"
    if (is.null(parameter)) parameter <- 3
    if (!(is_single_number(parameter) && parameter >= 1 && parameter == round(parameter))) {
      stop("ERROR: for k-means, network_clustering_parameter (the number of clusters) should be a whole number of at least 1.")
    }
    parameter <- as.character(as.integer(parameter))
  }

  if (algorithm == "mcl") {
    api_algorithm <- "MCL"
    if (is.null(parameter)) parameter <- 3
    if (!(is_single_number(parameter) && parameter >= 1.1 && parameter <= 10)) {
      stop("ERROR: for MCL, network_clustering_parameter (the inflation) should be a number from 1.1 to 10.")
    }
    parameter <- as.character(parameter)
  }

  if (algorithm == "leiden") {
    api_algorithm <- "leiden"
    if (is.null(parameter)) parameter <- 1
    if (!(is_single_number(parameter) && parameter >= 0.1 && parameter <= 10)) {
      stop("ERROR: for Leiden, network_clustering_parameter (the resolution) should be a number from 0.1 to 10.")
    }
    parameter <- as.character(parameter)
  }

  if (!is.null(edge_visibility)) {
    if (!(is.character(edge_visibility) && length(edge_visibility) == 1 && !is.na(edge_visibility) &&
      tolower(edge_visibility) %in% c("faded", "dotted", "solid", "noshow"))) {
      stop("ERROR: network_clustering_edge_visibility should be one of: 'faded', 'dotted', 'solid' or 'noshow'.")
    }
    edge_visibility <- tolower(edge_visibility)
  }

  if (!is.null(cluster_name_size)) {
    if (!(is_single_number(cluster_name_size) && cluster_name_size %in% c(1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6))) {
      stop("ERROR: cluster_name_size should be one of: 1, 1.5, 2, 2.5, 3, 3.5, 4, 5 or 6.")
    }
    cluster_name_size <- as.character(cluster_name_size)
  }

  # STRING names the parameter after the algorithm (network_clustering_parameter_kmeans, _mcl, _leiden)

  params <- list(network_clustering_algorithm = api_algorithm)
  params[[paste("network_clustering_parameter_", algorithm, sep = "")]] <- parameter
  params$cluster_aware_layout <- cluster_aware_layout
  params$network_clustering_edge_visibility <- edge_visibility
  params$show_cluster_names <- show_cluster_names
  params$cluster_name_size <- cluster_name_size

  return(params)
}


# STRING's evidence channels, by the names that the channels option of the
# network pictures and links takes, with the API parameter of each channel.
STRING_EVIDENCE_CHANNELS <- c(
  neighborhood = "channel1",
  fusion = "channel2",
  cooccurrence = "channel3",
  coexpression = "channel4",
  experiments = "channel5",
  databases = "channel6",
  textmining = "channel7"
)

# The names of the channels as the columns of the interaction tables with
# link_data = "detailed" (the column names of STRING's download files), which the
# channels option takes too.
STRING_EVIDENCE_CHANNEL_ALIASES <- c(
  cooccurence = "cooccurrence",
  experimental = "experiments",
  database = "databases"
)


# Checks the evidence channels of a network picture or link and returns them as
# STRING API parameters; an empty list without channels (NULL), for which
# STRING uses all channels, as before. STRING recomputes the combined score of
# each interaction from the selected channels and applies required_score to the
# recomputed score.
#   channels  a character vector of channel names (names(STRING_EVIDENCE_CHANNELS),
#             or the column names of STRING_EVIDENCE_CHANNEL_ALIASES, in any case);
#             a name given more than once counts once
# When any channel parameter is sent, STRING turns off the channels that are
# not sent, so all seven are sent: "on" for the selected channels and "off" for
# the others.
normalize_evidence_channels <- function(channels = NULL) {
  if (is.null(channels)) {
    return(list())
  }

  channel_names <- names(STRING_EVIDENCE_CHANNELS)
  valid_channels_text <- paste("'", channel_names, "'", sep = "", collapse = ", ")

  if (!(is.character(channels) && !any(is.na(channels)))) {
    stop("ERROR: channels should be a character vector of channel names: ", valid_channels_text, ".")
  }

  if (length(channels) == 0) {
    stop(
      "ERROR: channels should name at least one channel (NULL uses all channels): ",
      valid_channels_text, "."
    )
  }

  # a column name of the detailed interaction tables stands for its channel
  lower_case_channels <- tolower(channels)
  is_alias <- lower_case_channels %in% names(STRING_EVIDENCE_CHANNEL_ALIASES)
  lower_case_channels[is_alias] <- STRING_EVIDENCE_CHANNEL_ALIASES[lower_case_channels[is_alias]]

  is_known_channel <- lower_case_channels %in% channel_names
  if (!all(is_known_channel)) {
    unknown_channels <- unique(channels[!is_known_channel])
    stop(
      "ERROR: unknown channels: ", paste("'", unknown_channels, "'", sep = "", collapse = ", "),
      ". channels should be one or more of: ", valid_channels_text, " (or the column names of the detailed ",
      "interaction tables: 'cooccurence', 'experimental', 'database')."
    )
  }

  selected_channels <- unique(lower_case_channels)

  params <- list()
  for (channel_name in channel_names) {
    channel_param <- STRING_EVIDENCE_CHANNELS[[channel_name]]
    if (channel_name %in% selected_channels) {
      params[[channel_param]] <- "on"
    } else {
      params[[channel_param]] <- "off"
    }
  }

  return(params)
}


# The options that STRING's API has since version 12.5 (the enrichment filters, get_enrichment's add_scores,
# the enrichment figure's graph_type and sort_by, and the clustering of network pictures) are ignored without a
# word by older versions, such as version-12-0.string-db.org: a request with them would get unfiltered terms or
# a picture without clusters.
# Returns the options to send (those given), or none for an older version, with a warning when any was given.
# minimum_version (text, e.g. "12.0") is the first version that supports the options: 12.5 unless given. The
# evidence channels of network pictures and links (channel1 to channel7) work from version 12.0; versions 11.5
# and 11.0b ignore them without a word.
options_supported_by_version <- function(params, version, options_description, minimum_version = "12.5") {
  params <- drop_null_params(params)

  version_number <- suppressWarnings(as.numeric(version))
  minimum_version_number <- as.numeric(minimum_version)
  version_supports_options <- !is.na(version_number) && version_number >= minimum_version_number

  if (version_supports_options || length(params) == 0) {
    return(params)
  }

  warning(
    options_description, " need STRING version ", minimum_version, " or later; version ", version,
    " does not support them, so they are not used.",
    call. = FALSE
  )
  return(list())
}


# Stops, before any request, when an argument has one of the values that STRING
# understands only since version 12.5 (newer_values) and the object uses an
# older version. Such a value cannot be left out with a warning, as the options
# of options_supported_by_version are: STRING 12.0 and 11.x do not refuse it
# but silently use their default instead, so that, for example, an enrichment
# figure with category="Hallmark" would come back as a GO Process figure.
stop_on_value_needing_version <- function(value, newer_values, argument_name, version) {
  if (!(value %in% newer_values)) {
    return(invisible(NULL))
  }

  version_number <- suppressWarnings(as.numeric(version))
  version_supports_value <- !is.na(version_number) && version_number >= 12.5
  if (version_supports_value) {
    return(invisible(NULL))
  }

  stop(
    "ERROR: ", argument_name, " '", value, "' needs STRING version 12.5 or later; version ", version,
    " would silently use its default instead.",
    call. = FALSE
  )
}


is_igraph_directed <- function(graph) {
  if (exists("is_directed", envir = asNamespace("igraph"), inherits = FALSE)) {
    return(igraph::is_directed(graph))
  }

  return(igraph::is.directed(graph))
}


# delete column in data frame
delColDf <- function(df, colName) {
  if (colName %in% names(df)) {
    return(df[, -which(names(df) %in% c(colName))])
  } else {
    return(df)
  }
}


# download a file when it is not already present and also check the dimension of the file with the STRING server
downloadAbsentFileSTRING <- function(urlStr, oD = tempdir()) {
  expectedSize <- read.table(url(paste("http://string.uzh.ch/permanent_scripts/get_file_size.pl?file=", urlStr, sep = "")), stringsAsFactors = FALSE, fill = TRUE)$V1

  fileName <- tail(strsplit(urlStr, "/")[[1]], 1)
  temp <- paste(oD, "/", fileName, sep = "")
  if (!file.exists(temp) || file.info(temp)$size == 0 || file.info(temp)$size != expectedSize) download.file(urlStr, temp)
  if (file.info(temp)$size == 0) {
    unlink(temp)
    temp <- NULL
    cat(paste("ERROR: failed to download ", fileName, ".\nPlease check your internet connection and/or try again. ",
      "\nThen, if you still display this error message please contact us.",
      sep = ""
    ))
  }
  return(temp)
}


# download a file when it is not already present
# The file is downloaded under a temporary name in the same directory (<file name>.<random>.part) and
# gets its own name only when download.file() has finished. A download that is cut off part-way
# (download.file() stops with "Transferred a partial file" or a timeout) leaves the part that arrived in
# its file: under the file's own name, the next call would take that part for the whole file, as only
# an absent or empty file is downloaded again. The temporary file is removed when the download stops,
# also on an interrupt, and the error of download.file() goes on as before.
downloadAbsentFile <- function(urlStr, oD = tempdir()) {
  fileName <- tail(strsplit(urlStr, "/")[[1]], 1)
  temp <- paste(oD, "/", fileName, sep = "")
  if (!file.exists(temp) || file.info(temp)$size == 0) {
    partialFile <- tempfile(pattern = paste(fileName, ".", sep = ""), tmpdir = oD, fileext = ".part")
    on.exit(unlink(partialFile), add = TRUE)

    # R's option timeout (60 s unless set) limits the whole transfer, so a large STRING file that
    # arrives more slowly is cut off. As the help of download.file recommends for packages, the
    # download gets at least 300 s; a higher timeout of the user is kept. The option is restored
    # afterwards.
    previousOptions <- options(timeout = max(300, getOption("timeout")))
    on.exit(options(previousOptions), add = TRUE)

    # the request carries the package's caller identity, and the file keeps the name of the address
    # without it. mode = "wb": download.file chooses binary mode on Windows only for an address
    # that ends in .gz (or another binary extension), which an address with parameters does not.
    downloadStatus <- download.file(url_with_caller_identity(urlStr), partialFile, mode = "wb")

    # 0 is a finished download; the external methods ("wget" and "curl") return the non-zero exit
    # status of a failed download instead of stopping
    if (downloadStatus == 0) {
      unlink(temp)
      file.rename(partialFile, temp)
    }
  }
  if (!file.exists(temp) || file.info(temp)$size == 0) {
    unlink(temp)
    temp <- NULL
    cat(paste("ERROR: failed to download ", fileName, ".\nPlease check your internet connection and/or try again. ",
      "\nThen, if you still display this error message please contact us.",
      sep = ""
    ))
  }
  return(temp)
}



# Merge preserving the original order if sort==FALSE (this doesn't happen in the original merge implementation)
merge.with.order <- function(x, y, ..., sort = T) {
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  add.id.column.to.data <- function(DATA) {
    data.frame(DATA, id... = seq_len(nrow(DATA)))
  }
  # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
  order.by.id...and.remove.it <- function(DATA) {
    # gets in a data.frame with the "id..." column.  Orders by it and returns it
    if (!any(colnames(DATA) == "id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")

    ss_r <- order(DATA$id...)
    ss_c <- colnames(DATA) != "id..."
    DATA[ss_r, ss_c]
  }

  if (sort == F) {
    return(order.by.id...and.remove.it(merge(x = add.id.column.to.data(x), y = y, ..., sort = FALSE)))
  } else {
    return(merge(x = x, y = y, ..., sort = sort))
  }
}


# mapping function (add the possibility to map using more than one column)
# The identifiers are compared in upper case (unless caseSensitive), and an NA
# or empty identifier never maps. Every result row keeps the identifiers of its
# own input row, also when a later column mapped it or when it maps to several
# targets, and the rows keep the order of dfToMap (a row that maps to several
# targets is repeated in place). As before, the first column of strColsFrom
# comes first in the result, followed by the other columns of dfToMap and then
# by strColToDfMap.
multi_map_df <- function(dfToMap, dfMap, strColsFrom, strColFromDfMap, strColToDfMap, caseSensitive = FALSE) {
  # a matrix (which STRINGdb 2.24 and earlier also mapped) is mapped as a data frame
  if (is.matrix(dfToMap)) {
    dfToMap <- as.data.frame(dfToMap, stringsAsFactors = FALSE)
  }

  # merge() reorders and repeats rows, so every row carries the number of its
  # input row: the identifiers are restored, and the rows ordered, by it
  input_row_col <- "input_row..."
  dfToMap[[input_row_col]] <- seq_len(nrow(dfToMap))

  # merge() matches NA to NA and "" to "", so an NA or empty identifier is given
  # a key that no alias has, and stays unmapped. The aliases are left as they
  # are: the rows of an identifier with several targets keep the order of before.
  unmappable_key <- "\001no identifier\001"

  original_ids <- list()
  for (i in seq_along(strColsFrom)) {
    original_ids[[i]] <- dfToMap[, strColsFrom[i]]
    if (!caseSensitive) {
      original_ids[[i]] <- as.vector(original_ids[[i]])
      dfToMap[, strColsFrom[i]] <- normalize_alias_key(dfToMap[, strColsFrom[i]])
    }

    missing_ids <- is.na(dfToMap[, strColsFrom[i]]) | dfToMap[, strColsFrom[i]] == ""
    if (any(missing_ids)) {
      dfToMap[, strColsFrom[i]] <- as.character(dfToMap[, strColsFrom[i]])
      dfToMap[missing_ids, strColsFrom[i]] <- unmappable_key
    }
  }
  if (!caseSensitive) {
    dfMap[, strColFromDfMap] <- normalize_alias_key(dfMap[, strColFromDfMap])
  }

  dfMap2 <- unique(subset(dfMap, select = c(strColFromDfMap, strColToDfMap)))
  df2 <- merge.with.order(dfToMap, dfMap2, by.x = strColsFrom[1], by.y = strColFromDfMap, all.x = TRUE, sort = FALSE)
  if (length(strColsFrom) > 1) {
    for (i in 2:length(strColsFrom)) {
      dfna <- delColDf(subset(df2, is.na(as.vector(df2[, strColToDfMap]))), strColToDfMap)
      dfgood <- subset(df2, !is.na(as.vector(df2[, strColToDfMap])))
      df3 <- merge.with.order(dfna, dfMap2, by.x = strColsFrom[i], by.y = strColFromDfMap, all.x = TRUE, sort = FALSE)
      df2 <- rbind(dfgood, df3)
    }
  }

  # every row gets back the identifiers of its own input row, as given (not
  # upper-cased, and without the key of the NA and empty identifiers)
  for (i in seq_along(strColsFrom)) {
    df2[, strColsFrom[i]] <- original_ids[[i]][df2[[input_row_col]]]
  }

  df2 <- df2[order(df2[[input_row_col]]), , drop = FALSE]
  df2[[input_row_col]] <- NULL

  return(df2)
}



## Rename column data frame
renameColDf <- function(df, colOldName, colNewName) {
  if (!(colOldName %in% names(df))) print(paste("ERROR: We cannot find ", colOldName, " in the data frame.", sep = ""))
  names(df)[which(names(df) == colOldName)] <- colNewName
  return(df)
}


############################################################################################
## STRING's values/ranks enrichment (get_values_ranks_enrichment): the "Proteins with
## Values/Ranks" analysis of the STRING website, a GSEA-like gene set enrichment analysis of a
## whole ranked experiment that is computed on STRING's servers, not locally, with STRING's own
## statistics (Kolmogorov-Smirnov and aggregate fold change tests, not the running-sum score of
## Subramanian et al.). It runs on STRING's job
## queue: get_api_key gives an anonymous key, valuesranks_enrichment_submit queues a job,
## valuesranks_enrichment_status reports it, and the results of a finished job are
## downloaded from its download_url (api/tsv/downloadenrichmentresults). The JSON methods
## answer their errors with HTTP 200 and a record whose status is "error".
############################################################################################

# The highest FDR cutoff that get_values_ranks_enrichment sends (ge_fdr). STRING accepts up to 1,
# but a cutoff above 0.25 defeats the optimizations of the computation and can give huge results.
VALUES_RANKS_MAX_FDR <- 0.25

# The columns of STRING's results table (api/tsv/downloadenrichmentresults), as STRING writes them.
VALUES_RANKS_STRING_COLUMNS <- c(
  "category", "termID", "termDescription", "genesMapped", "genesInSet",
  "enrichmentScore", "direction", "falseDiscoveryRate", "method", "proteinIDs",
  "proteinLabels", "proteinInputLabels", "proteinInputValues", "proteinRanks"
)

# The names that the package gives STRING's columns (names: STRING's), in the order of the results:
# those of get_enrichment where a column means the same, so that code for its results also works for
# these. proteinInputLabels is left out: the package submits STRING identifiers, which STRING gives
# back there, as in proteinIDs.
VALUES_RANKS_COLUMN_NAMES <- c(
  category = "category", termID = "term", genesMapped = "number_of_genes",
  genesInSet = "number_of_genes_in_background", proteinIDs = "inputGenes", proteinLabels = "preferredNames",
  enrichmentScore = "enrichment_score", direction = "direction", method = "method",
  falseDiscoveryRate = "fdr", termDescription = "description", proteinInputValues = "inputValues",
  proteinRanks = "inputRanks"
)

# The columns of the results that the package returns (format_values_ranks_results).
VALUES_RANKS_RESULT_COLUMNS <- unname(VALUES_RANKS_COLUMN_NAMES)

# The category codes of get_enrichment for the category names of STRING's values/ranks results
# (the inverse of shortNameOfEnrichmentCategory of STRING's server). "Monarch" stands for all
# phenotype ontologies, whose code comes from the prefix of the term
# (VALUES_RANKS_PHENOTYPE_PREFIXES).
VALUES_RANKS_CATEGORY_CODES <- c(
  "GO Process" = "Process", "GO Function" = "Function", "GO Component" = "Component",
  "UniProt Keywords" = "Keyword", "KEGG" = "KEGG", "Reactome" = "RCTM", "GWAS" = "GWAS",
  "Hallmark" = "Hallmark", "Pfam" = "Pfam", "SMART" = "SMART", "InterPro" = "InterPro",
  "Publications" = "PMID", "STRING clusters" = "NetworkNeighborAL", "COMPARTMENTS" = "COMPARTMENTS",
  "TISSUES" = "TISSUES", "DISEASES" = "DISEASES", "WikiPathways" = "WikiPathways"
)

# The phenotype ontology of a "Monarch" term, by the prefix of the term (as on STRING's server;
# "WBP:" is the website's short form of "WBPhenotype:").
VALUES_RANKS_PHENOTYPE_PREFIXES <- c(
  "HP:" = "HPO", "MP:" = "MPO", "FBcv:" = "DPO", "WBPhenotype:" = "WPO", "WBP:" = "WPO",
  "ZP:" = "ZPO", "FYPO:" = "FYPO"
)

# The first words of the states of a finished job and of a failed one, as STRING reports
# them (lower case, without dots; see values_ranks_state), and of a job that waits in STRING's
# queue. "expired" is a job that STRING no longer has. Any other state counts as running.
VALUES_RANKS_FINISHED_STATES <- c("success", "done")
VALUES_RANKS_FAILED_STATES <- c("error", "failure", "failed")
VALUES_RANKS_WAITING_STATES <- c("submitted", "preparing")

# The end of the messages of a timeout: STRING's servers may be busy, so a timeout that repeats
# is for STRING's support.
VALUES_RANKS_SUPPORT_HINT <- "If this keeps happening, please contact STRING support: https://string-db.org/cgi/help"


# A count with its noun: "1 job", "2 jobs".
count_text <- function(n, singular, plural = paste(singular, "s", sep = "")) {
  if (n == 1) {
    return(paste(n, singular))
  }
  return(paste(n, plural))
}


# Why STRING did not take values/ranks jobs, and what to do, from STRING's refusal (string_message),
# after lead (e.g. "STRING did not take them within 7200 seconds"). STRING refuses the jobs of an
# API key that already has its maximum of 1000 jobs waiting or running ("Job submission limit
# reached ..."): then the user's own jobs have to finish first. Other refusals mean that STRING is
# busy: then to try again later, and to contact STRING support if it keeps happening. Returns a
# list with text (sentences) and advice (a sentence).
values_ranks_refusal_explanation <- function(lead, string_message) {
  if (grepl("limit reached", string_message, ignore.case = TRUE)) {
    return(list(
      text = paste(lead, ", because this API key already has its maximum of 1000 jobs waiting or running.", sep = ""),
      advice = "Submit again once some of your jobs have finished."
    ))
  }
  return(list(
    text = paste(lead, ". STRING's answer: ", sub("\\.$", "", trimws(string_message)), ".", sep = ""),
    advice = paste("STRING's servers may be busy: please try again later.", VALUES_RANKS_SUPPORT_HINT)
  ))
}

# The first and the longest wait between two status requests, in seconds.
VALUES_RANKS_FIRST_POLL_INTERVAL <- 5
VALUES_RANKS_LONGEST_POLL_INTERVAL <- 30

# The number of status requests in a row that may fail (e.g. a connection error) before
# waiting gives up; the job itself keeps running on STRING.
VALUES_RANKS_MAX_FAILED_STATUS_REQUESTS <- 5

# STRING's answers to a submission that it cannot take now but may take later: the limit of
# active jobs per API key (1000 jobs waiting or running), the limits of its queue per address and
# in total, and a queue that refused the job. A synchronous call submits such an experiment again
# once jobs have finished.
VALUES_RANKS_RETRY_LATER_PATTERN <- "limit reached|too many submissions|too many jobs|try again later"

# The number of experiments named in a warning about failed experiments (with their reasons, it
# stays under R's limit of 1000 characters for a warning).
VALUES_RANKS_EXPERIMENTS_IN_WARNING <- 3

# The number of job ids that the message of a wait that ends early lists; more are saved in a file
# (values_ranks_jobs_hint).
VALUES_RANKS_JOB_IDS_IN_MESSAGE <- 20



# The types of the result columns that are not text. The others are read as text, so that
# a term or label such as "NA" or "TRUE" stays as STRING wrote it.
VALUES_RANKS_RESULT_TYPES <- c(
  genesMapped = "integer", genesInSet = "integer",
  enrichmentScore = "numeric", falseDiscoveryRate = "numeric"
)


# POSTs a form to a JSON method of the STRING API and returns the parsed answer (a list of
# records). STRING sends JSON as text/json, so the type is given to content(). Server errors
# (HTTP 5xx) and refused requests (HTTP 4xx) stop with STRING's explanation; an error record
# in the answer is left to the caller.
post_string_json <- function(url_str, params, timeout_seconds = 60) {
  response <- POST(url = url_str, body = with_caller_identity(params), config = timeout(timeout_seconds))
  stop_on_server_error(response, url_str)
  stop_on_client_error(response, url_str)
  return(content(response, as = "parsed", type = "application/json", encoding = "UTF-8"))
}


# Reads STRING's explanation from a TSV error answer ("Error<TAB>ErrorMessage" and a line
# with both), as the download of expired results gives. Returns NULL for other text.
string_tsv_error_message <- function(body_text) {
  if (is.null(body_text) || length(body_text) != 1 || is.na(body_text)) {
    return(NULL)
  }

  lines <- strsplit(body_text, "\r?\n")[[1]]
  if (length(lines) < 2 || !grepl("^Error\tErrorMessage", lines[1])) {
    return(NULL)
  }

  fields <- strsplit(lines[2], "\t", fixed = TRUE)[[1]]
  explanation <- paste(fields[nchar(trimws(fields)) > 0], collapse = ": ")
  explanation <- decode_html_entities(gsub("<[^>]*>", " ", explanation))
  explanation <- trimws(gsub("[[:space:]]+", " ", explanation))
  if (nchar(explanation) == 0) {
    return(NULL)
  }
  return(explanation)
}


# GETs a text answer of STRING (the TSV results of a values/ranks job); query and the package's
# caller identity are added to the parameters already in url_str. An expired or unknown result
# may be answered with HTTP 400 and a TSV error, which stops with its explanation.
get_string_text <- function(url_str, query = NULL, timeout_seconds = 300) {
  response <- GET(url_str, config = timeout(timeout_seconds), query = with_caller_identity(query))
  stop_on_server_error(response, url_str)

  body_text <- content(response, as = "text", encoding = "UTF-8")
  status <- status_code(response)
  if (status >= 400) {
    explanation <- string_tsv_error_message(body_text)
    if (is.null(explanation)) {
      explanation <- string_api_error_message(body_text)
    }
    if (is.null(explanation)) {
      explanation <- "STRING did not say why."
    }
    stop("ERROR: STRING refused the request with HTTP status ", status, " for ", url_str, ": ", explanation, call. = FALSE)
  }
  return(body_text)
}


# The first record of a JSON answer of the values/ranks methods (a list of records), or NULL
# for an empty answer.
first_json_record <- function(answer) {
  if (is.list(answer) && length(answer) > 0 && is.list(answer[[1]])) {
    return(answer[[1]])
  }
  if (is.list(answer) && length(answer) > 0 && !is.null(names(answer))) {
    return(answer)
  }
  return(NULL)
}


# A field of a JSON record as one character string ("" when it is missing or null).
json_record_field <- function(record, field_name) {
  value <- record[[field_name]]
  if (is.null(value) || length(value) == 0) {
    return("")
  }
  return(as.character(value[[1]]))
}


# STRING's explanation of an error answer, with advice for the messages about API keys.
values_ranks_error_message <- function(message) {
  if (grepl("Authorizing the api key", message, fixed = TRUE)) {
    return(paste(
      "STRING is still activating this API key (\"", message, "\"). Activating a new key can take up to ",
      "15 minutes; please try again later.",
      sep = ""
    ))
  }
  if (grepl("Api key does not exist|Api key not provided", message)) {
    return(paste(
      message, " If STRINGDB_API_KEY is set, check its key, or unset it to use the key ",
      "that the package keeps on this computer (see get_api_key).",
      sep = ""
    ))
  }
  if (nchar(message) == 0) {
    return("STRING did not say why.")
  }
  return(message)
}


# How to get the results of a values/ranks job later, for the messages of a job that may
# still be running or whose results were not downloaded.
values_ranks_resume_hint <- function(job_id) {
  return(paste("Get its results later with string_db$get_values_ranks_results(\"", job_id, "\").", sep = ""))
}


# The state of a values/ranks job: the first word of STRING's status, in lower case and
# without dots ("failure." gives "failure"), or "" for an empty status.
values_ranks_state <- function(status) {
  words <- strsplit(tolower(trimws(status)), "[[:space:].]+")[[1]]
  words <- words[nchar(words) > 0]
  if (length(words) == 0) {
    return("")
  }
  return(words[1])
}


# The status of a values/ranks job as get_values_ranks_status reports it, one of a fixed set,
# from STRING's own state, which varies (e.g. "error", "failure" and "expired" for a failed
# job): "submitted" (waiting in STRING's queue), "running", "success" (finished) or "failed".
values_ranks_status_of <- function(status) {
  state <- values_ranks_state(status)
  if (state %in% VALUES_RANKS_FINISHED_STATES) {
    return("success")
  }
  if (state %in% c(VALUES_RANKS_FAILED_STATES, "expired")) {
    return("failed")
  }
  if (state %in% VALUES_RANKS_WAITING_STATES) {
    return("submitted")
  }
  return("running")
}


# The error of a status request that STRING refused (a key that did not submit the job), or
# that it answered without the job's state. Asking again cannot help, so the wait stops at
# once instead of counting it as a failed request.
values_ranks_refused_error <- function(message_text) {
  return(structure(
    class = c("values_ranks_refused", "error", "condition"),
    list(message = message_text, call = NULL)
  ))
}


# Whether an error of a request comes from an interrupt (Ctrl-C) during the request: curl
# may then abort the transfer with an error instead of passing the interrupt on (libcurl's
# CURLE_ABORTED_BY_CALLBACK, whose message depends on the curl version).
is_interrupted_request <- function(condition) {
  if (inherits(condition, "curl_error_aborted_by_callback")) {
    return(TRUE)
  }
  condition_text <- conditionMessage(condition)
  return(grepl("aborted by an application callback|Callback aborted", condition_text))
}


# The file in which the package keeps the STRING API key of this computer: one line in the
# package's configuration directory of the user (tools::R_user_dir; the environment variable
# R_USER_CONFIG_DIR moves it, e.g. in the tests).
values_ranks_key_file <- function() {
  return(file.path(tools::R_user_dir("STRINGdb", which = "config"), "api_key"))
}


# The key saved in key_file, or "" when there is none or it cannot be read.
read_saved_api_key <- function(key_file = values_ranks_key_file()) {
  if (!file.exists(key_file)) {
    return("")
  }
  first_line <- tryCatch(
    readLines(key_file, n = 1, warn = FALSE),
    error = function(e) character(0),
    warning = function(w) character(0)
  )
  if (length(first_line) == 0 || is.na(first_line[1])) {
    return("")
  }
  return(trimws(first_line[1]))
}


# Saves api_key in key_file, readable only by its owner, and returns the key that key_file holds
# afterwards ("" when nothing could be saved, e.g. in a home directory without write access). The
# key is written to a temporary file next to key_file, which then takes its place. With
# replace = FALSE a key file that another R session has written meanwhile is kept, so that the
# sessions of a computer that request their first key at the same time end up with one key: the
# temporary file gets the name of key_file by a hard link, which fails for an existing file.
save_api_key <- function(api_key, key_file = values_ranks_key_file(), replace = TRUE) {
  previous_umask <- Sys.umask("077")
  on.exit(Sys.umask(previous_umask), add = TRUE)

  dir.create(dirname(key_file), recursive = TRUE, showWarnings = FALSE)
  temporary_file <- tempfile("api_key_", tmpdir = dirname(key_file))
  on.exit(unlink(temporary_file), add = TRUE)

  written <- tryCatch(
    {
      writeLines(api_key, temporary_file)
      Sys.chmod(temporary_file, mode = "0600")
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!isTRUE(written)) {
    return("")
  }

  if (replace) {
    suppressWarnings(file.rename(temporary_file, key_file))
  } else {
    linked <- suppressWarnings(file.link(temporary_file, key_file))
    if (!linked && nchar(read_saved_api_key(key_file)) == 0) {
      # this file system has no hard links, or the key file holds no key (e.g. it is empty): the
      # file is renamed instead
      suppressWarnings(file.rename(temporary_file, key_file))
    }
  }
  return(read_saved_api_key(key_file))
}


# Requests a new API key from STRING with the package's caller identity, with which STRING makes
# the key work at once, saves it in key_file for this computer and returns it. When another R
# session of the computer has saved a key meanwhile, that key is used instead.
request_and_save_api_key <- function(stable_url, quiet = FALSE, key_file = values_ranks_key_file(),
                                     send = post_string_json) {
  key <- request_string_api_key(stable_url, send)

  saved_key <- save_api_key(key$api_key, key_file, replace = FALSE)
  if (nchar(saved_key) == 0) {
    warning(
      "The STRING API key ", key$api_key, " could not be saved in ", key_file, ", so a later R session would ",
      "request another one. To keep it, set STRINGDB_API_KEY=", key$api_key, " in your ~/.Renviron file.",
      call. = FALSE
    )
    return(key$api_key)
  }

  if (!quiet) {
    message(
      "Requested an API key from STRING and saved it in ", key_file, ". It is used for all ",
      "values/ranks jobs on this computer (STRINGDB_API_KEY, when set, is used instead)."
    )
  }
  return(saved_key)
}


# Returns the API key for the values/ranks methods, as a list with the key and where it comes
# from ("environment", "saved" or "new"): the environment variable STRINGDB_API_KEY when it is
# set; otherwise the key saved on this computer (values_ranks_key_file). Without either,
# request_new = TRUE requests a key from STRING and saves it (request_and_save_api_key), so that a
# computer keeps using one key. STRING keeps each job for the key that submitted it, so the methods
# that read jobs give request_new = FALSE and stop instead. The key is not an argument of the
# methods: another key (e.g. of the jobs of another computer) is given as STRINGDB_API_KEY.
resolve_values_ranks_api_key <- function(stable_url = "", request_new = TRUE, quiet = FALSE,
                                         key_file = values_ranks_key_file(), send = post_string_json) {
  environment_key <- trimws(Sys.getenv("STRINGDB_API_KEY"))
  if (nchar(environment_key) > 0) {
    return(list(key = environment_key, source = "environment"))
  }

  saved_key <- read_saved_api_key(key_file)
  if (nchar(saved_key) > 0) {
    return(list(key = saved_key, source = "saved"))
  }

  if (!request_new) {
    stop(
      "ERROR: there is no STRING API key on this computer. STRING keeps each values/ranks job for the API ",
      "key that submitted it: set that key as the environment variable STRINGDB_API_KEY, e.g. with ",
      "Sys.setenv(STRINGDB_API_KEY = \"...\").",
      call. = FALSE
    )
  }

  new_key <- request_and_save_api_key(stable_url, quiet, key_file, send)
  return(list(key = new_key, source = "new"))
}


# Checks the options of get_values_ranks_enrichment and returns them as parameters of
# valuesranks_enrichment_submit.
#   max_fdr         the FDR cutoff of the reported terms (ge_fdr): above 0, at most 0.25
#   rank_direction  "descending" (rank 1 is the largest value, and "top" the end of the
#                   largest values) or "ascending" (rank 1 is the smallest value). It only
#                   names the two ends of the ranked input and numbers the ranks; the
#                   enrichment is the same (ge_enrichment_rank_direction -1 or 1; STRING's
#                   own default, without the parameter, is ascending).
normalize_values_ranks_options <- function(max_fdr, rank_direction) {
  if (!(is.numeric(max_fdr) && length(max_fdr) == 1 && !is.na(max_fdr) && max_fdr > 0 && max_fdr <= VALUES_RANKS_MAX_FDR)) {
    stop("ERROR: max_fdr should be a number above 0 and at most ", VALUES_RANKS_MAX_FDR, ".", call. = FALSE)
  }

  if (!(is.character(rank_direction) && length(rank_direction) == 1 && !is.na(rank_direction) &&
    rank_direction %in% c("descending", "ascending"))) {
    stop("ERROR: rank_direction should be 'descending' or 'ascending'.", call. = FALSE)
  }

  rank_direction_param <- "1"
  if (rank_direction == "descending") {
    rank_direction_param <- "-1"
  }

  return(list(ge_fdr = as.character(max_fdr), ge_enrichment_rank_direction = rank_direction_param))
}


# Prepares the input of a values/ranks job: one line per protein, its identifier and its
# value separated by a tab, as valuesranks_enrichment_submit expects. Rows without an
# identifier or without a value (NA) are left out, and so are infinite values (counted apart,
# for a warning: see warn_infinite_values_ranks); an identifier given more than once keeps its
# value farthest from 0 (its first such value). Returns a list with the text and the counts of
# the proteins submitted and of the rows left out.
prepare_values_ranks_input <- function(string_ids, values) {
  if (length(string_ids) != length(values)) {
    stop("ERROR: string_ids and values should have the same length.", call. = FALSE)
  }
  if (is.logical(values) && length(values) > 0 && all(is.na(values))) {
    stop("ERROR: the values are all NA: there is nothing to rank.", call. = FALSE)
  }
  if (!is.numeric(values)) {
    stop("ERROR: values should be numeric, e.g. log fold changes or test statistics.", call. = FALSE)
  }

  string_ids <- as.character(string_ids)
  if (any(grepl("[\t\r\n]", string_ids))) {
    stop("ERROR: string_ids should not contain tabs or line breaks.", call. = FALSE)
  }

  has_identifier <- !is.na(string_ids) & nchar(trimws(string_ids)) > 0
  n_infinite <- sum(has_identifier & is.infinite(values))
  usable_rows <- has_identifier & is.finite(values)
  n_missing <- sum(!usable_rows) - n_infinite
  string_ids <- trimws(string_ids[usable_rows])
  values <- values[usable_rows]

  # order() is stable, so among equal absolute values the first one is kept
  rows_by_size <- order(-abs(values))
  kept_rows <- sort(rows_by_size[!duplicated(string_ids[rows_by_size])])
  n_duplicates <- length(string_ids) - length(kept_rows)
  string_ids <- string_ids[kept_rows]
  values <- values[kept_rows]

  if (length(string_ids) == 0) {
    stop("ERROR: no identifier with a finite value is left to submit.", call. = FALSE)
  }

  input_text <- paste(string_ids, "\t", as.character(values), sep = "", collapse = "\n")

  return(list(
    text = input_text,
    n_submitted = length(string_ids),
    n_missing = n_missing,
    n_infinite = n_infinite,
    n_duplicates = n_duplicates
  ))
}


# Warns that infinite values were left out of the experiments (prepare_values_ranks_experiments),
# also with quiet = TRUE: they are typically the most significant proteins, e.g. -log10 of a
# p-value of 0, so leaving them out changes the analysis.
warn_infinite_values_ranks <- function(experiments) {
  n_infinite <- vapply(experiments, function(experiment) as.numeric(experiment$n_infinite), numeric(1))
  if (sum(n_infinite) == 0) {
    return(invisible(NULL))
  }

  left_out <- paste(sum(n_infinite), "infinite values were left out")
  with_proteins <- ", with their proteins"
  if (sum(n_infinite) == 1) {
    left_out <- "1 infinite value was left out"
    with_proteins <- ", with its protein"
  }
  where <- ""
  if (length(experiments) > 1) {
    where <- paste(" in ", sum(n_infinite > 0), " of ", length(experiments), " experiments", sep = "")
  }
  warning(
    left_out, where, with_proteins, " (e.g. -log10 of a p-value of 0). To keep these proteins, give them ",
    "finite values, e.g. the largest finite value of the same sign.",
    call. = FALSE
  )
  return(invisible(NULL))
}


# The error of a submission that STRING did not accept. It keeps STRING's own message
# (string_message), so that a limit that allows a later submission can be told from other
# refusals (is_retry_later_refusal).
values_ranks_submission_error <- function(string_message) {
  return(structure(
    class = c("values_ranks_submission_refused", "error", "condition"),
    list(
      message = paste(
        "ERROR: STRING did not accept the values/ranks job: ", values_ranks_error_message(string_message),
        sep = ""
      ),
      string_message = string_message,
      call = NULL
    )
  ))
}


# Whether STRING refused a submission only for now (VALUES_RANKS_RETRY_LATER_PATTERN), e.g. because
# the API key has its limit of active jobs.
is_retry_later_refusal <- function(condition) {
  if (!inherits(condition, "values_ranks_submission_refused")) {
    return(FALSE)
  }
  return(grepl(VALUES_RANKS_RETRY_LATER_PATTERN, condition$string_message, ignore.case = TRUE))
}


# Whether STRING refused a submission because of the API key (an unknown key, or one that STRING
# is still activating): no other job can then be submitted with it.
is_api_key_refusal <- function(condition) {
  if (!inherits(condition, "values_ranks_submission_refused")) {
    return(FALSE)
  }
  return(grepl("Api key does not exist|Api key not provided|Authorizing the api key", condition$string_message))
}


# Submits a values/ranks job and returns its job_id, or stops with STRING's reason (a
# values_ranks_submission_refused error).
submit_values_ranks_job <- function(stable_url, species, input_text, api_key, options, send = post_string_json) {
  url_str <- paste(stable_url, "/api/json/valuesranks_enrichment_submit", sep = "")
  params <- c(
    list(
      api_key = api_key,
      species = format(species, scientific = FALSE, trim = TRUE),
      identifiers = input_text
    ),
    options
  )

  record <- first_json_record(send(url_str, params))
  if (is.null(record)) {
    stop("ERROR: STRING did not answer the submission of the values/ranks job.", call. = FALSE)
  }

  job_id <- json_record_field(record, "job_id")
  if (json_record_field(record, "status") == "error" || nchar(job_id) == 0) {
    stop(values_ranks_submission_error(json_record_field(record, "message")))
  }
  return(job_id)
}


# Submits the first job of a call with the key of key_info (resolve_values_ranks_api_key) and
# returns a list with the job_id and the key_info used. A saved key that STRING does not know (twice:
# for the submission and for a status request) is replaced by a new key (source "replacement"),
# which is saved in place of the saved key only once STRING has taken a job with it: a key that
# STRING refused for another reason, e.g. during an outage of its key database, is then not
# overwritten by a key that does not work either. When
# the submission with the replacement fails, the error carries the replacement (key_info), so
# that the caller goes on with it instead of requesting yet another key.
submit_first_values_ranks_job <- function(stable_url, species, input_text, key_info, options,
                                          quiet = FALSE, key_file = values_ranks_key_file(), send = post_string_json) {
  if (key_info$source != "replacement") {
    first_try <- tryCatch(
      submit_values_ranks_job(stable_url, species, input_text, key_info$key, options, send),
      values_ranks_submission_refused = function(e) e
    )
    if (!inherits(first_try, "values_ranks_submission_refused")) {
      return(list(job_id = first_try, key_info = key_info))
    }

    saved_key_unknown <- key_info$source == "saved" &&
      grepl("Api key does not exist", first_try$string_message, fixed = TRUE)
    if (!saved_key_unknown) {
      stop(first_try)
    }

    # STRING also answers "does not exist" when it cannot reach its key database, so it is asked
    # about the key once more (a status request) before the key is replaced: when it knows the
    # key now, the job is submitted again with it, and when the answer is unclear, nothing changes
    key_known <- tryCatch(
      {
        get_values_ranks_jobs_states(stable_url, key_info$key, send)
        TRUE
      },
      values_ranks_refused = function(e) !grepl("Api key does not exist", conditionMessage(e), fixed = TRUE),
      error = function(e) NA
    )
    if (isTRUE(key_known)) {
      job_id <- submit_values_ranks_job(stable_url, species, input_text, key_info$key, options, send)
      return(list(job_id = job_id, key_info = key_info))
    }
    if (is.na(key_known)) {
      stop(first_try)
    }

    if (!quiet) {
      message(
        "STRING does not know the API key saved in ", key_file, " (", key_info$key, "). A new key is ",
        "requested, and saved in its place once STRING has taken a job with it."
      )
    }
    new_key <- request_string_api_key(stable_url, send)$api_key
    key_info <- list(key = new_key, source = "replacement", replaced_key = key_info$key)
  }

  job_id <- tryCatch(
    submit_values_ranks_job(stable_url, species, input_text, key_info$key, options, send),
    error = function(e) {
      e$key_info <- key_info
      stop(e)
    }
  )

  # the replacement is announced with a warning, which also shows with quiet = TRUE: the jobs of
  # the old key are read only with that key
  if (save_api_key(key_info$key, key_file, replace = TRUE) != key_info$key) {
    warning(
      "STRING did not know the API key ", key_info$replaced_key, " saved in ", key_file, ", and the new key ",
      key_info$key, " could not be saved there. To keep it, set STRINGDB_API_KEY=", key_info$key,
      " in your ~/.Renviron file.",
      call. = FALSE
    )
  } else {
    warning(
      "STRING did not know the API key ", key_info$replaced_key, " saved in ", key_file, "; it is replaced by ",
      "the new key ", key_info$key, ".",
      call. = FALSE
    )
  }
  key_info$source <- "new"
  return(list(job_id = job_id, key_info = key_info))
}


# Submits the job of one experiment (get_values_ranks_enrichment with a vector of values) as
# submit_first_values_ranks_job does, but when STRING cannot take it now (is_retry_later_refusal,
# e.g. the limit of active jobs of the API key), submits it again until STRING takes it or
# timeout_seconds have passed, waiting between the tries from 5 s, growing to 30 s. Returns what
# submit_first_values_ranks_job returns, and waited, the seconds that the submission took.
submit_values_ranks_job_when_possible <- function(stable_url, species, input_text, key_info, options,
                                                  timeout_seconds, quiet = FALSE, key_file = values_ranks_key_file(),
                                                  send = post_string_json, sleep = Sys.sleep,
                                                  clock = function() proc.time()[["elapsed"]]) {
  started <- clock()
  interval <- VALUES_RANKS_FIRST_POLL_INTERVAL
  told <- FALSE

  repeat {
    submission <- tryCatch(
      submit_first_values_ranks_job(stable_url, species, input_text, key_info, options, quiet, key_file, send),
      error = function(e) e
    )
    if (!inherits(submission, "error")) {
      submission$waited <- clock() - started
      return(submission)
    }
    if (!is_retry_later_refusal(submission)) {
      stop(submission)
    }

    # a replacement key that STRING has not taken yet is kept for the next try
    if (!is.null(submission$key_info)) {
      key_info <- submission$key_info
    }
    if (clock() - started >= timeout_seconds) {
      explanation <- values_ranks_refusal_explanation(
        paste("STRING did not take the values/ranks job within", timeout_seconds, "seconds"),
        submission$string_message
      )
      stop("ERROR: ", explanation$text, " ", explanation$advice, call. = FALSE)
    }
    if (!quiet && !told) {
      message(
        "STRING cannot take the values/ranks job now (", submission$string_message, "); it is submitted as ",
        "soon as STRING takes it."
      )
      told <- TRUE
    }
    sleep(interval)
    interval <- min(VALUES_RANKS_LONGEST_POLL_INTERVAL, interval * 1.5)
  }
}


# Asks STRING for the state of a values/ranks job. The request is a POST, so that no cache
# keeps a state (STRING sends max-age=86400) and the key is not part of a URL. Returns a
# list with status (lower case, without a final "."), message and the addresses of a
# finished job. A request that STRING refuses, which it answers with an error record without
# a job_id, stops with a values_ranks_refused error, and so does an answer without a record.
# STRING keeps each job for the key that submitted it, and that key was accepted by the
# submission, so a refusal means that another key was given (e.g. to get_values_ranks_results); a new
# key cannot help. An answer without a record comes from a job that STRING has confirmed for
# this key but whose state it lost: the job failed on STRING.
get_values_ranks_job_status <- function(stable_url, api_key, job_id, send = post_string_json) {
  url_str <- paste(stable_url, "/api/json/valuesranks_enrichment_status", sep = "")
  params <- list(api_key = api_key, job_id = job_id)

  record <- first_json_record(send(url_str, params))
  if (is.null(record)) {
    stop(values_ranks_refused_error(paste(
      "ERROR: STRING gave no state for values/ranks job ", job_id, ": the job may have failed on STRING, ",
      "or this API key did not submit it (STRING keeps each job for the key that submitted it). ",
      "If it failed, please submit it again.",
      sep = ""
    )))
  }

  status <- sub("\\.$", "", tolower(trimws(json_record_field(record, "status"))))
  message_text <- json_record_field(record, "message")

  # a failed job is reported with its job_id; a refused request without one
  if (status == "error" && nchar(json_record_field(record, "job_id")) == 0) {
    explanation <- message_text
    if (nchar(explanation) == 0) {
      explanation <- "STRING did not say why."
    }
    explanation <- paste(
      explanation, " STRING keeps each job for the API key that submitted it; please use the same key ",
      "as for the submission (STRINGDB_API_KEY or the key saved on this computer).",
      sep = ""
    )
    stop(values_ranks_refused_error(paste(
      "ERROR: STRING refused to report the values/ranks job ", job_id, ": ", explanation,
      sep = ""
    )))
  }

  return(list(
    status = status,
    message = message_text,
    page_url = json_record_field(record, "page_url"),
    download_url = json_record_field(record, "download_url"),
    graph_url = json_record_field(record, "graph_url"),
    string_version = json_record_field(record, "string_version")
  ))
}


# Waits until a values/ranks job has finished and returns its state, or stops: when the job
# failed or expired, when STRING refuses to report it, when timeout_seconds have passed, or
# after several failed status requests in a row. Every stop for a job that may still be
# running says how to get its results later. The requests are spaced from 5 s, growing to
# 30 s. A progress message is printed when the state or its message changes (not only a
# number in it, such as a position in the queue).
wait_for_values_ranks_job <- function(stable_url, api_key, job_id, timeout_seconds, quiet = FALSE,
                                      send = post_string_json, sleep = Sys.sleep,
                                      clock = function() proc.time()[["elapsed"]]) {
  resume_hint <- values_ranks_resume_hint(job_id)
  started <- clock()
  interval <- VALUES_RANKS_FIRST_POLL_INTERVAL
  failed_requests <- 0
  last_report <- ""

  repeat {
    job <- tryCatch(
      get_values_ranks_job_status(stable_url, api_key, job_id, send),
      error = function(e) e
    )

    if (inherits(job, "values_ranks_refused")) {
      stop(job)
    }

    if (inherits(job, "error")) {
      if (is_interrupted_request(job)) {
        stop("ERROR: the wait for values/ranks job ", job_id, " was interrupted. It keeps running on STRING. ",
          resume_hint,
          call. = FALSE
        )
      }

      failed_requests <- failed_requests + 1
      if (failed_requests >= VALUES_RANKS_MAX_FAILED_STATUS_REQUESTS) {
        stop(
          "ERROR: the state of values/ranks job ", job_id, " could not be read from STRING (",
          conditionMessage(job), "). The job may still be running. ", resume_hint,
          call. = FALSE
        )
      }
    } else {
      failed_requests <- 0
      state <- values_ranks_state(job$status)

      if (state %in% VALUES_RANKS_FINISHED_STATES) {
        return(job)
      }
      if (state %in% VALUES_RANKS_FAILED_STATES) {
        stop(
          "ERROR: STRING's values/ranks job ", job_id, " failed: ", values_ranks_error_message(job$message),
          call. = FALSE
        )
      }
      if (state == "expired") {
        stop("ERROR: STRING no longer has values/ranks job ", job_id, ". Please submit it again.", call. = FALSE)
      }

      report <- paste(job$status, gsub("[0-9]+", "", job$message))
      if (!quiet && report != last_report) {
        details <- ""
        if (nchar(job$message) > 0) {
          details <- paste(" (", job$message, ")", sep = "")
        }
        message("STRING job ", job_id, ": ", job$status, details)
        last_report <- report
      }
    }

    if (clock() - started >= timeout_seconds) {
      stop(
        "ERROR: values/ranks job ", job_id, " did not finish within ", timeout_seconds, " seconds. ",
        "It keeps running on STRING. ", resume_hint, " STRING's servers may be busy. ", VALUES_RANKS_SUPPORT_HINT,
        call. = FALSE
      )
    }

    sleep(interval)
    interval <- min(VALUES_RANKS_LONGEST_POLL_INTERVAL, interval * 1.5)
  }
}


# Reads the TSV results of a values/ranks job into a data frame in the package's form
# (format_values_ranks_results), the counts as integers and the scores as numbers. A table
# without terms (only its header) gives a data frame without rows. An empty answer is
# STRING's answer for results it no longer has, and stops.
read_values_ranks_results <- function(body_text, job_id) {
  return(format_values_ranks_results(read_values_ranks_table(body_text, job_id)))
}


# The category codes of get_enrichment for the categories of values/ranks results (category, as
# STRING names them; terms, their term identifiers): VALUES_RANKS_CATEGORY_CODES, and for
# "Monarch" the phenotype ontology of the term's prefix (VALUES_RANKS_PHENOTYPE_PREFIXES). A
# category that neither knows keeps STRING's name.
values_ranks_category_codes <- function(categories, terms) {
  codes <- unname(VALUES_RANKS_CATEGORY_CODES[categories])

  is_phenotype <- !is.na(categories) & categories == "Monarch"
  for (prefix in names(VALUES_RANKS_PHENOTYPE_PREFIXES)) {
    has_prefix <- is_phenotype & !is.na(terms) & startsWith(terms, prefix)
    codes[has_prefix] <- VALUES_RANKS_PHENOTYPE_PREFIXES[[prefix]]
  }

  unknown <- is.na(codes)
  codes[unknown] <- categories[unknown]
  return(codes)
}


# A comma-separated list of numbers for each text of texts, as a list of vectors made by convert
# (as.numeric or as.integer); an empty text gives an empty vector.
split_number_lists <- function(texts, convert) {
  return(lapply(texts, function(text) {
    if (is.na(text) || nchar(text) == 0) {
      return(convert(character(0)))
    }
    return(convert(strsplit(text, ",", fixed = TRUE)[[1]]))
  }))
}


# The results of a values/ranks job (STRING's table, read_values_ranks_table) in the package's form,
# aligned with get_enrichment: its column names and order where a column means the same
# (VALUES_RANKS_COLUMN_NAMES) and its category codes (values_ranks_category_codes). inputGenes and
# preferredNames stay comma-separated text, as in get_enrichment; the input values and the ranks of
# the term's proteins become list columns of numbers (inputValues, numeric; inputRanks, integer).
format_values_ranks_results <- function(table) {
  missing_columns <- setdiff(names(VALUES_RANKS_COLUMN_NAMES), colnames(table))
  if (length(missing_columns) > 0) {
    stop(
      "ERROR: STRING's results of the values/ranks job lack the columns ", paste(missing_columns, collapse = ", "), ".",
      call. = FALSE
    )
  }

  results <- table[, names(VALUES_RANKS_COLUMN_NAMES), drop = FALSE]
  colnames(results) <- unname(VALUES_RANKS_COLUMN_NAMES)
  results$category <- values_ranks_category_codes(results$category, results$term)
  results$inputValues <- split_number_lists(results$inputValues, as.numeric)
  results$inputRanks <- split_number_lists(results$inputRanks, as.integer)
  rownames(results) <- NULL
  return(results)
}


# Reads the TSV results of a values/ranks job into a data frame with STRING's columns
# (VALUES_RANKS_STRING_COLUMNS), the counts as integers and the scores as numbers; the other
# columns stay text.
read_values_ranks_table <- function(body_text, job_id) {
  if (is.null(body_text) || length(body_text) != 1 || is.na(body_text) || nchar(trimws(body_text)) == 0) {
    stop(
      "ERROR: STRING has no results for values/ranks job ", job_id, " (they may have expired). ",
      "Please submit the job again.",
      call. = FALSE
    )
  }

  first_line <- strsplit(body_text, "\r?\n")[[1]][1]
  header <- strsplit(first_line, "\t", fixed = TRUE)[[1]]
  if (length(header) < 2 || header[1] != "category" || header[2] != "termID") {
    stop(
      "ERROR: STRING's answer for the results of values/ranks job ", job_id, " is not a results table: ",
      substr(body_text, 1, 200),
      call. = FALSE
    )
  }

  # the text columns stay text; types are given only for the columns in the header, since
  # read.table warns about the others
  column_types <- rep("character", length(header))
  names(column_types) <- header
  typed_columns <- intersect(names(VALUES_RANKS_RESULT_TYPES), header)
  column_types[typed_columns] <- VALUES_RANKS_RESULT_TYPES[typed_columns]

  # comment.char = "": term descriptions may contain "#"; na.strings: no text means "missing"
  return(read.table(
    text = body_text, sep = "\t", header = TRUE, quote = "", comment.char = "",
    colClasses = column_types, na.strings = character(0),
    stringsAsFactors = FALSE, fill = TRUE, check.names = FALSE
  ))
}


# Gives the results of a values/ranks job the job's identifier and addresses as attributes
# (job_id, page_url, graph_url, download_url, string_version); job is the job's state
# (get_values_ranks_job_status).
set_values_ranks_attributes <- function(results, job_id, job) {
  attr(results, "job_id") <- job_id
  attr(results, "page_url") <- job$page_url
  attr(results, "graph_url") <- job$graph_url
  attr(results, "download_url") <- job$download_url
  attr(results, "string_version") <- job$string_version
  return(results)
}


# Waits for a values/ranks job, downloads its results and returns them, with the job's
# addresses as attributes (job_id, page_url, graph_url, download_url, string_version).
# A download that fails is tried again a few times, as for several jobs (run_values_ranks_jobs).
# When the wait or the download is interrupted (Ctrl-C) or the download keeps failing, a message
# says how to get the results later.
collect_values_ranks_results <- function(stable_url, api_key, job_id, timeout_seconds, quiet = FALSE,
                                         send = post_string_json, fetch = get_string_text, sleep = Sys.sleep,
                                         clock = function() proc.time()[["elapsed"]]) {
  resume_hint <- values_ranks_resume_hint(job_id)

  job <- withCallingHandlers(
    wait_for_values_ranks_job(stable_url, api_key, job_id, timeout_seconds, quiet, send, sleep, clock),
    interrupt = function(condition) {
      message("\nThe values/ranks job ", job_id, " keeps running on STRING. ", resume_hint)
    }
  )

  if (nchar(job$download_url) == 0) {
    stop("ERROR: STRING gave no address for the results of values/ranks job ", job_id, ".", call. = FALSE)
  }

  # the results are downloaded at once, as STRING keeps them only for a limited time. A download
  # that fails (also one that gives no results table, e.g. an error page) is tried again, waiting
  # between the tries from 5 s, growing to 30 s.
  interval <- VALUES_RANKS_FIRST_POLL_INTERVAL
  attempt <- 1
  repeat {
    results <- withCallingHandlers(
      tryCatch(download_values_ranks_results(job, job_id, fetch), error = function(e) e),
      interrupt = function(condition) {
        message("\nThe results of values/ranks job ", job_id, " were not downloaded. ", resume_hint)
      }
    )
    if (!inherits(results, "error")) {
      break
    }
    if (is_interrupted_request(results)) {
      stop("ERROR: the download of the results of values/ranks job ", job_id, " was interrupted. ", resume_hint,
           call. = FALSE)
    }
    if (attempt >= VALUES_RANKS_MAX_FAILED_STATUS_REQUESTS) {
      stop(conditionMessage(results), " ", resume_hint, " If STRING no longer has them, please submit the job again.",
           call. = FALSE)
    }
    attempt <- attempt + 1
    sleep(interval)
    interval <- min(VALUES_RANKS_LONGEST_POLL_INTERVAL, interval * 1.5)
  }

  if (!quiet) {
    message("STRING's values/ranks enrichment found ", nrow(results), " terms (job ", job_id, ").")
  }
  return(results)
}


# Requests an API key from STRING (get_api_key). Returns a list with the key and STRING's note.
# STRING makes the key work at once, as post_string_json sends the package's caller identity.
request_string_api_key <- function(stable_url, send = post_string_json) {
  url_str <- paste(stable_url, "/api/json/get_api_key", sep = "")
  record <- first_json_record(send(url_str, list()))
  api_key <- ""
  if (!is.null(record)) {
    api_key <- json_record_field(record, "api_key")
  }
  if (nchar(api_key) == 0) {
    stop("ERROR: STRING did not give an API key. Please try again later.", call. = FALSE)
  }
  return(list(api_key = api_key, note = json_record_field(record, "note")))
}


############################################################################################
## Several experiments at once: one values/ranks job for each experiment (a column of a table
## of values). All jobs are submitted before the wait, so that STRING's queue runs them side by
## side; an API key may have up to 1000 jobs waiting or running at once. One status request
## reports all jobs of the key, and the results of each job are downloaded as soon as it has
## finished, as STRING keeps them only for a limited time.
############################################################################################

# Checks the timeout of the values/ranks methods: a positive number of seconds.
check_values_ranks_timeout <- function(timeout) {
  if (!(is.numeric(timeout) && length(timeout) == 1 && !is.na(timeout) && timeout > 0)) {
    stop("ERROR: timeout should be a positive number of seconds.", call. = FALSE)
  }
  return(invisible(NULL))
}


# Checks the quiet argument of the values/ranks methods: TRUE or FALSE.
check_values_ranks_quiet <- function(quiet) {
  if (!(is.logical(quiet) && length(quiet) == 1 && !is.na(quiet))) {
    stop("ERROR: quiet should be TRUE or FALSE.", call. = FALSE)
  }
  return(invisible(NULL))
}


# Stops, before any request, when the object uses a STRING version older than 12.0: STRING's
# values/ranks API and its API keys exist since version 12.0, and older versions answer with a
# web page (11.0) or with "You have not provided any input" (11.5).
check_values_ranks_version <- function(version) {
  version_number <- suppressWarnings(as.numeric(version))
  if (!is.na(version_number) && version_number >= 12.0) {
    return(invisible(NULL))
  }
  stop(
    "ERROR: STRING's values/ranks enrichment needs STRING version 12.0 or later; this object uses version ",
    version, ". Create the object with a newer version, e.g. STRINGdb$new(version = \"12.5\", species = ...).",
    call. = FALSE
  )
}


# The names of values that could be identifiers: the names of a vector (or of a 1-d array, e.g. from
# tapply), or the row names of a matrix or data frame. Row names that are the numbers of the rows (a
# data frame's automatic or subset row names, which it keeps as integers, or a matrix's digits only)
# are not identifiers. NULL without such names.
values_ranks_names_of <- function(values) {
  if (is.matrix(values) || is.data.frame(values)) {
    if (is.data.frame(values) && !is.character(.row_names_info(values, type = 0L))) {
      return(NULL)
    }
    row_names <- rownames(values)
    if (is.null(row_names) || all(grepl("^[0-9]+$", row_names))) {
      return(NULL)
    }
    return(row_names)
  }
  return(names(values))
}


# The identifiers and values of the values/ranks methods, which take them as string_ids and values,
# or the values alone (the other one NULL), as R's GSEA packages do (e.g. fgsea's stats): a numeric
# vector named by STRING identifiers (also a named 1-d array, e.g. from tapply), or a matrix or data
# frame with STRING identifiers as row names and one column for each experiment. Row names that are
# the numbers of the rows are not identifiers, and stop. Returns a list with string_ids and values.
values_ranks_input_of <- function(string_ids = NULL, values = NULL) {
  how_to_give <- paste(
    "give string_ids and values, or the values alone: a numeric vector named by STRING identifiers ",
    "(e.g. setNames(mapped$logFC, mapped$STRING_id)), or a matrix or data frame with STRING identifiers ",
    "as row names and one column for each experiment.",
    sep = ""
  )

  if (!is.null(string_ids) && !is.null(values)) {
    # string_ids that are values themselves: the second argument was probably meant for another
    # option (e.g. max_fdr), given by position
    if (is.numeric(string_ids) || is.matrix(string_ids) || is.data.frame(string_ids)) {
      stop(
        "ERROR: string_ids should be STRING identifiers, but are values. To give the values alone, give the ",
        "other options by name (e.g. max_fdr = 0.05); otherwise ", how_to_give,
        call. = FALSE
      )
    }

    # string_ids and values are paired by position. Values named by STRING identifiers in another
    # order (e.g. a vector sorted by value, as clusterProfiler's geneList) would give each protein
    # the value of another, so they stop; names that are other identifiers (e.g. gene names) are
    # not compared.
    value_names <- values_ranks_names_of(values)
    identifiers <- as.character(string_ids)
    if (!is.null(value_names) && any(value_names %in% identifiers) && !identical(as.character(value_names), identifiers)) {
      stop(
        "ERROR: the values are named by STRING identifiers that differ from string_ids (in their order or in ",
        "which they are), while values and string_ids are paired by position. Give the values alone (their ",
        "names are then the identifiers), or the values in the order of string_ids.",
        call. = FALSE
      )
    }
    return(list(string_ids = string_ids, values = values))
  }

  ranked <- values
  if (is.null(ranked)) {
    ranked <- string_ids
  }
  if (is.null(ranked)) {
    stop("ERROR: no values to rank: ", how_to_give, call. = FALSE)
  }

  # a vector, or a 1-d array (e.g. from tapply), whose names are its dimnames
  if (is.numeric(ranked) && length(dim(ranked)) <= 1) {
    if (is.null(names(ranked))) {
      stop("ERROR: the values have no names, which would be their STRING identifiers: ", how_to_give, call. = FALSE)
    }
    return(list(string_ids = names(ranked), values = as.vector(ranked)))
  }

  if (is.matrix(ranked) || is.data.frame(ranked)) {
    row_names <- values_ranks_names_of(ranked)
    if (is.null(row_names)) {
      stop(
        "ERROR: the table of values has no row names that could be STRING identifiers (it has the numbers of ",
        "its rows): ", how_to_give,
        call. = FALSE
      )
    }
    return(list(string_ids = row_names, values = ranked))
  }

  if (is.logical(ranked) && length(ranked) > 0 && all(is.na(ranked))) {
    stop("ERROR: the values are all NA: there is nothing to rank.", call. = FALSE)
  }
  stop("ERROR: the values should be numeric: ", how_to_give, call. = FALSE)
}


# Prepares the experiments of the values/ranks methods. values is a numeric vector, one
# experiment, or a matrix or data frame with one numeric column for each experiment, named by
# its column, and one row for each identifier of string_ids. Returns a list with experiments,
# named, and several, whether values was a table (whose results are a list, even with one
# column). Each experiment has the counts of prepare_values_ranks_input and make_text, which
# makes the input text of its job: the texts of a table are made one at a time, when its jobs are
# submitted, since they would take much memory for many experiments (about 0.8 MB for 20,000
# proteins). All experiments are checked first, so that a problem stops before any submission.
# Infinite values left out get a warning (warn_infinite_values_ranks).
prepare_values_ranks_experiments <- function(string_ids, values) {
  if (!(is.matrix(values) || is.data.frame(values))) {
    input <- prepare_values_ranks_input(string_ids, values)
    input_text <- input$text
    input$make_text <- function() input_text
    warn_infinite_values_ranks(list(input))
    return(list(experiments = list(input), several = FALSE))
  }

  if (nrow(values) != length(string_ids)) {
    stop("ERROR: values should have one row for each identifier of string_ids.", call. = FALSE)
  }
  if (ncol(values) == 0) {
    stop("ERROR: values should have a column for each experiment, but has no columns.", call. = FALSE)
  }

  # an experiment without a column name is named by its column number
  experiment_names <- colnames(values)
  if (is.null(experiment_names)) {
    experiment_names <- rep("", ncol(values))
  }
  unnamed_columns <- is.na(experiment_names) | nchar(trimws(experiment_names)) == 0
  experiment_names[unnamed_columns] <- paste("experiment", which(unnamed_columns), sep = "_")
  experiment_names <- make.unique(experiment_names)

  if (is.data.frame(values)) {
    numeric_columns <- vapply(values, is.numeric, logical(1))
  } else {
    numeric_columns <- rep(is.numeric(values), ncol(values))
  }
  if (!all(numeric_columns)) {
    stop(
      "ERROR: each column of values should be numeric, one for each experiment; these are not: ",
      paste(experiment_names[!numeric_columns], collapse = ", "), ".",
      call. = FALSE
    )
  }

  column_values <- function(column_number) {
    if (is.data.frame(values)) {
      return(values[[column_number]])
    }
    return(values[, column_number])
  }

  # the function that makes the input text of a column: its environment keeps only the column
  # number (the values and identifiers are those of this call), so that no text is kept
  text_maker <- function(column_number) {
    force(column_number)
    return(function() prepare_values_ranks_input(string_ids, column_values(column_number))$text)
  }

  # an experiment keeps its column number and counts, not its text
  experiment_of <- function(column_number) {
    input <- tryCatch(
      prepare_values_ranks_input(string_ids, column_values(column_number)),
      error = function(e) {
        stop(
          "ERROR: experiment ", experiment_names[column_number], ": ", sub("^ERROR: ", "", conditionMessage(e)),
          call. = FALSE
        )
      }
    )
    return(list(
      n_submitted = input$n_submitted,
      n_missing = input$n_missing,
      n_infinite = input$n_infinite,
      n_duplicates = input$n_duplicates,
      column_number = column_number,
      make_text = text_maker(column_number)
    ))
  }

  experiments <- lapply(seq_len(ncol(values)), experiment_of)
  names(experiments) <- experiment_names
  warn_infinite_values_ranks(experiments)
  return(list(experiments = experiments, several = TRUE))
}


# Messages about the rows that the experiments leave out (prepare_values_ranks_input): for one
# experiment as before, for several in one message each about missing values and about repeated
# identifiers. Infinite values get a warning instead (warn_infinite_values_ranks).
report_left_out_rows <- function(prepared) {
  n_missing <- vapply(prepared$experiments, function(input) as.numeric(input$n_missing), numeric(1))
  n_duplicates <- vapply(prepared$experiments, function(input) as.numeric(input$n_duplicates), numeric(1))

  if (!prepared$several) {
    if (n_missing > 0) {
      message("Left out ", count_text(n_missing, "row"), " without an identifier or without a value.")
    }
    if (n_duplicates > 0) {
      message("Left out ", count_text(n_duplicates, "repeated identifier"), " (each keeps its value farthest from 0).")
    }
    return(invisible(NULL))
  }

  if (sum(n_missing) > 0) {
    message(
      "Left out rows without an identifier or without a value in ", sum(n_missing > 0), " of ",
      length(n_missing), " experiments (", sum(n_missing), " rows in all)."
    )
  }
  if (sum(n_duplicates) > 0) {
    message(
      "Left out repeated identifiers in ", sum(n_duplicates > 0), " of ", length(n_duplicates),
      " experiments (each keeps its value farthest from 0)."
    )
  }
  return(invisible(NULL))
}


# How to get the results of several values/ranks jobs later: a call with their job ids, named by
# their experiments. More than VALUES_RANKS_JOB_IDS_IN_MESSAGE job ids, which would make a long
# message (about 32 KB for 1000 jobs), are saved in a file of the R session's temporary directory,
# which the call reads.
values_ranks_jobs_hint <- function(job_ids) {
  if (length(job_ids) > VALUES_RANKS_JOB_IDS_IN_MESSAGE) {
    job_ids_file <- tempfile("values_ranks_job_ids_", fileext = ".rds")
    saved <- tryCatch(
      {
        saveRDS(job_ids, job_ids_file)
        TRUE
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (saved) {
      # forward slashes, so that the path can be pasted into R also on Windows
      job_ids_path <- normalizePath(job_ids_file, winslash = "/")
      return(paste(
        "Their ", length(job_ids), " job ids are saved in ", job_ids_path, " until this R session ends: get their ",
        "results later with string_db$get_values_ranks_results(readRDS(\"", job_ids_path, "\")).",
        sep = ""
      ))
    }
  }
  job_ids_text <- paste(deparse(unclass(job_ids), width.cutoff = 500L), collapse = "")
  return(paste("Get their results later with string_db$get_values_ranks_results(", job_ids_text, ").", sep = ""))
}


# A text shortened to at most `width` characters.
shorten_text <- function(text, width) {
  return(ifelse(nchar(text) > width, paste(substr(text, 1, width - 3), "...", sep = ""), text))
}


# The text of a warning about experiments or jobs, with the reason for each (reasons, named): the
# first VALUES_RANKS_EXPERIMENTS_IN_WARNING of them, names shortened to 40 and reasons to 120
# characters, and how many more there are. It stays short enough for R's limit on the length of a
# warning (1000 characters). what_for_one is what, for one of them (e.g. "experiment failed").
values_ranks_experiments_warning <- function(what, reasons, what_for_one = what) {
  shown <- reasons[seq_len(min(length(reasons), VALUES_RANKS_EXPERIMENTS_IN_WARNING))]
  shown_items <- paste(shorten_text(names(shown), 40), ": ", shorten_text(unname(shown), 120), sep = "")
  text <- paste(count_text(length(reasons), what_for_one, what), ": ", paste(shown_items, collapse = "; "), sep = "")
  if (length(reasons) > length(shown)) {
    text <- paste(text, "; and ", length(reasons) - length(shown), " more", sep = "")
  }
  return(text)
}


# Checks job ids for the methods that read jobs: a character vector of job identifiers, named
# or not (e.g. what submit_values_ranks_enrichment returns). Returns them without surrounding
# spaces and without other attributes; named job ids get unique names, and a job id without a
# name is named by itself.
check_values_ranks_job_ids <- function(job_ids) {
  is_job_ids <- is.character(job_ids) && length(job_ids) > 0 && !any(is.na(job_ids)) &&
    all(nchar(trimws(job_ids)) > 0)
  if (!is_job_ids) {
    stop(
      "ERROR: job_ids should be one or more job identifiers, e.g. what submit_values_ranks_enrichment returns.",
      call. = FALSE
    )
  }

  job_names <- names(job_ids)
  job_ids <- trimws(as.vector(job_ids))
  if (!is.null(job_names)) {
    unnamed_jobs <- is.na(job_names) | nchar(job_names) == 0
    job_names[unnamed_jobs] <- job_ids[unnamed_jobs]
    names(job_ids) <- make.unique(job_names)
  }
  return(job_ids)
}


# The states of all values/ranks jobs of an API key, from one status request: without a job_id,
# STRING reports every job of the key. Returns a list named by job_id with, for each job, what
# get_values_ranks_job_status returns for one job, and its creation_time. A request that STRING
# refuses (an error record without a job_id, e.g. for an unknown key) stops with a
# values_ranks_refused error.
get_values_ranks_jobs_states <- function(stable_url, api_key, send = post_string_json) {
  url_str <- paste(stable_url, "/api/json/valuesranks_enrichment_status", sep = "")
  answer <- send(url_str, list(api_key = api_key))

  # the answer is a list of records, or one record
  records <- list()
  if (is.list(answer) && length(answer) > 0) {
    if (is.list(answer[[1]])) {
      records <- answer
    } else if (!is.null(names(answer))) {
      records <- list(answer)
    }
  }
  records <- Filter(is.list, records)

  job_ids <- vapply(records, function(record) json_record_field(record, "job_id"), character(1))
  statuses <- vapply(
    records,
    function(record) sub("\\.$", "", tolower(trimws(json_record_field(record, "status")))),
    character(1)
  )

  refusals <- which(nchar(job_ids) == 0 & statuses == "error")
  if (length(refusals) > 0) {
    explanation <- json_record_field(records[[refusals[1]]], "message")
    if (nchar(explanation) == 0) {
      explanation <- "STRING did not say why."
    }
    stop(values_ranks_refused_error(paste(
      "ERROR: STRING refused to report the values/ranks jobs of this API key: ", explanation,
      " STRING keeps each job for the API key that submitted it; please use the same key as for the ",
      "submission (STRINGDB_API_KEY or the key saved on this computer).",
      sep = ""
    )))
  }

  # the states are made in one pass, as an answer may list many jobs
  listed <- nchar(job_ids) > 0
  if (!any(listed)) {
    return(list())
  }
  states <- Map(
    function(record, status) {
      list(
        status = status,
        message = json_record_field(record, "message"),
        page_url = json_record_field(record, "page_url"),
        download_url = json_record_field(record, "download_url"),
        graph_url = json_record_field(record, "graph_url"),
        string_version = json_record_field(record, "string_version"),
        creation_time = json_record_field(record, "creation_time")
      )
    },
    records[listed], statuses[listed]
  )
  names(states) <- job_ids[listed]
  return(states)
}


# The states of values/ranks jobs (job_ids, named or not; states from get_values_ranks_jobs_states)
# as a data frame with the columns experiment (the name of the job, NA without names), job_id,
# status (one of a fixed set: values_ranks_status_of, or "unknown" for a job that STRING does not
# list for the key), message (STRING's) and creation_time. Without job_ids (NULL), all jobs that
# STRING lists for the key, oldest first.
values_ranks_status_table <- function(job_ids, states) {
  if (is.null(job_ids)) {
    # STRING's creation times are text such as "2026-11-05 12:00:00", which sorts in time order
    creation_times <- vapply(states, function(job) as.character(job$creation_time), character(1))
    job_ids <- as.character(names(states))[order(creation_times)]
  }
  experiment <- names(job_ids)
  if (is.null(experiment)) {
    experiment <- rep(NA_character_, length(job_ids))
  }
  status <- rep("unknown", length(job_ids))
  message_text <- rep(
    "STRING does not list this job for this API key: it was removed, or another key submitted it.",
    length(job_ids)
  )
  creation_time <- rep(NA_character_, length(job_ids))

  positions <- match(unname(job_ids), names(states))
  for (job_number in which(!is.na(positions))) {
    job <- states[[positions[job_number]]]
    status[job_number] <- values_ranks_status_of(job$status)
    message_text[job_number] <- job$message
    if (values_ranks_state(job$status) == "expired") {
      message_text[job_number] <- "STRING no longer has this job: submit it again."
    }
    creation_time[job_number] <- job$creation_time
  }

  return(data.frame(
    experiment = experiment, job_id = unname(job_ids), status = status, message = message_text,
    creation_time = creation_time, stringsAsFactors = FALSE
  ))
}


# Downloads and reads the results of a finished values/ranks job (job: its state) and returns
# them with the job's attributes (set_values_ranks_attributes). Stops when they cannot be had; an
# interrupt of the download (is_interrupted_request) stops with curl's own error.
download_values_ranks_results <- function(job, job_id, fetch = get_string_text) {
  if (nchar(job$download_url) == 0) {
    stop("ERROR: STRING gave no address for the results of values/ranks job ", job_id, ".", call. = FALSE)
  }
  body_text <- tryCatch(
    fetch(job$download_url),
    error = function(e) {
      if (is_interrupted_request(e)) {
        stop(e)
      }
      stop(
        "ERROR: the results of values/ranks job ", job_id, " could not be downloaded from STRING (",
        sub("^ERROR: ", "", conditionMessage(e)), ").",
        call. = FALSE
      )
    }
  )
  return(set_values_ranks_attributes(read_values_ranks_results(body_text, job_id), job_id, job))
}


# Submits one experiment (with its make_text) and returns a list with outcome: "submitted" (with
# job_id), "failed" (STRING refused this experiment; reason), "held_back" (STRING cannot take it
# now; reason, and request_failed when the request itself failed) or "stop" (STRING refused the
# API key, so that no further experiment can be submitted; reason); and key_info, which
# submit_first_values_ranks_job may have replaced. first_submission: no job of the call is
# submitted yet; a refusal of that first job stops with STRING's error, as it means a problem of
# all experiments (e.g. a wrong key or species), and so does a request that fails. An interrupt
# of the request (is_interrupted_request) stops with curl's error.
submit_next_values_ranks_experiment <- function(stable_url, species, experiment, key_info, options,
                                                first_submission = TRUE,
                                                quiet = FALSE, key_file = values_ranks_key_file(),
                                                send = post_string_json) {
  input_text <- experiment$make_text()

  submission <- tryCatch(
    {
      if (first_submission) {
        submit_first_values_ranks_job(
          stable_url, species, input_text, key_info, options, quiet, key_file, send
        )
      } else {
        new_job_id <- submit_values_ranks_job(stable_url, species, input_text, key_info$key, options, send)
        list(job_id = new_job_id, key_info = key_info)
      }
    },
    error = function(e) e
  )

  if (!inherits(submission, "error")) {
    return(list(outcome = "submitted", job_id = submission$job_id, key_info = submission$key_info, reason = ""))
  }

  # a replacement key that STRING has not taken yet is kept for the next submission
  if (!is.null(submission$key_info)) {
    key_info <- submission$key_info
  }
  reason <- sub("^ERROR: ", "", conditionMessage(submission))
  is_refusal <- inherits(submission, "values_ranks_submission_refused")

  if (is_interrupted_request(submission)) {
    stop(submission)
  }
  if (is_retry_later_refusal(submission)) {
    # STRING's own message, e.g. "Job submission limit reached. Please wait for your jobs to finish ..."
    return(list(outcome = "held_back", reason = submission$string_message, request_failed = FALSE, key_info = key_info))
  }
  if (first_submission) {
    stop(submission)
  }
  if (!is_refusal) {
    return(list(outcome = "held_back", reason = reason, request_failed = TRUE, key_info = key_info))
  }
  if (is_api_key_refusal(submission)) {
    return(list(outcome = "stop", reason = reason, key_info = key_info))
  }
  return(list(outcome = "failed", reason = reason, key_info = key_info))
}


# Runs values/ranks jobs: submits the experiments of pending (a named list of experiments of
# prepare_values_ranks_experiments) and, with wait = TRUE, waits for them and for the jobs of
# job_ids (named) and downloads their results. Experiments that STRING cannot take now are
# submitted as jobs finish. With wait = FALSE the run ends after the submissions (a submission
# request that fails is tried again a few times, as with waiting).
# Returns a list with job_ids (all submitted jobs, named), results (the downloaded results,
# named), failed (the reasons of the experiments that failed, named), pending (the experiments not
# submitted), held_back (STRING's reason why they were not), held_back_text and held_back_advice
# (that reason in sentences for the warnings, and what to do), timed_out (whether the timeout ended the wait for a
# job or for a free slot) and submission_timed_out (whether experiments were not submitted for the
# timeout). The timeout is per job: the wait for a job ends timeout_seconds after its submission
# to STRING (for the jobs of job_ids, after the start of the run), and an experiment that STRING
# cannot take waits as long for a free slot, from STRING's first refusal. A job that fails does
# not stop the others, and a download that fails is tried again in the next rounds. When the run ends early with an error (an interrupt, a lost
# connection, a refusal of the API key), a message says how to get the results later: with
# job_ids_known = TRUE (the caller gave the job ids) by asking again with them, otherwise with
# the job ids, which the message lists.
run_values_ranks_jobs <- function(stable_url, species, pending, key_info, options = list(),
                                  job_ids = character(0), wait = TRUE, timeout_seconds = 7200,
                                  quiet = FALSE, job_ids_known = FALSE,
                                  key_file = values_ranks_key_file(), send = post_string_json,
                                  fetch = get_string_text, sleep = Sys.sleep,
                                  clock = function() proc.time()[["elapsed"]]) {
  started <- clock()
  interval <- VALUES_RANKS_FIRST_POLL_INTERVAL
  failed_status_requests <- 0
  failed_submission_requests <- 0
  download_failures <- integer(0)
  results <- list()
  failed <- character(0)
  held_back <- ""
  # why experiments were not submitted, in sentences for the warnings, and what to do
  held_back_text <- ""
  held_back_advice <- ""
  timed_out <- FALSE
  submission_timed_out <- FALSE
  last_report <- ""
  first_submission <- length(job_ids) == 0
  n_experiments <- length(job_ids) + length(pending)

  # the start of each job's timeout (its submission; for the jobs of job_ids the start of the
  # run), the start of the wait for a free slot, and the jobs whose wait has ended
  submitted_at <- rep(started, length(job_ids))
  names(submitted_at) <- names(job_ids)
  held_back_since <- NA_real_
  given_up <- character(0)

  # FALSE once the submissions are given up (the requests keep failing, or STRING refused the
  # API key): the experiments left are not submitted, and the wait goes on for the submitted jobs
  submitting <- TRUE

  unfinished <- function() {
    return(setdiff(names(job_ids), c(names(results), names(failed), given_up)))
  }

  resume_hint <- function() {
    if (job_ids_known) {
      return("Ask again later with the same job ids: string_db$get_values_ranks_results(job_ids).")
    }
    return(values_ranks_jobs_hint(job_ids))
  }

  # ends the run with an error; the submitted jobs keep running on STRING
  stop_early <- function(reason) {
    if (length(job_ids) == 0) {
      stop("ERROR: ", reason, call. = FALSE)
    }
    message(resume_hint())
    stop(
      "ERROR: ", reason, " The submitted values/ranks jobs keep running on STRING; the message above ",
      "says how to get their results later.",
      call. = FALSE
    )
  }

  withCallingHandlers(
    repeat {
      # submissions: job_ids grows with each job, so that an interrupt lists all submitted jobs
      if (submitting && length(pending) > 0) {
        submitted_now <- 0
        request_failed <- FALSE
        while (length(pending) > 0) {
          experiment_name <- names(pending)[1]
          step <- tryCatch(
            submit_next_values_ranks_experiment(
              stable_url, species, pending[[1]], key_info, options, first_submission,
              quiet, key_file, send
            ),
            error = function(e) e
          )
          if (inherits(step, "error")) {
            if (is_interrupted_request(step)) {
              stop_early("The submission of the values/ranks jobs was interrupted.")
            }
            stop(step)
          }

          key_info <- step$key_info
          if (step$outcome == "held_back") {
            held_back <- step$reason
            if (!step$request_failed) {
              # STRING answered (it cannot take the job now): the connection works
              failed_submission_requests <- 0
              if (is.na(held_back_since)) {
                held_back_since <- clock()
              }
              explanation <- values_ranks_refusal_explanation("STRING did not take them now", step$reason)
              held_back_text <- explanation$text
              held_back_advice <- explanation$advice
              break
            }
            failed_submission_requests <- failed_submission_requests + 1
            request_failed <- TRUE
            if (failed_submission_requests >= VALUES_RANKS_MAX_FAILED_STATUS_REQUESTS) {
              submitting <- FALSE
              held_back <- paste("the submissions to STRING kept failing: ", step$reason, sep = "")
              held_back_text <- paste("The submissions to STRING kept failing: ", sub("\\.$", "", step$reason), ".", sep = "")
              held_back_advice <- "Please try again later."
            }
            break
          }
          if (step$outcome == "stop") {
            submitting <- FALSE
            held_back <- step$reason
            held_back_text <- paste(sub("\\.$", "", step$reason), ".", sep = "")
            held_back_advice <- ""
            break
          }

          failed_submission_requests <- 0
          if (step$outcome == "submitted") {
            job_ids[experiment_name] <- step$job_id
            # the job's timeout starts at its submission, and the wait for a free slot is over
            submitted_at[experiment_name] <- clock()
            held_back_since <- NA_real_
            first_submission <- FALSE
            submitted_now <- submitted_now + 1
            if (!quiet && submitted_now %% 100 == 0) {
              message("Submitted ", length(job_ids), " of ", n_experiments, " jobs to STRING's values/ranks enrichment...")
            }
          } else {
            failed[experiment_name] <- step$reason
          }
          held_back <- ""
          held_back_text <- ""
          held_back_advice <- ""
          pending <- pending[-1]
        }

        if (!quiet && submitted_now > 0) {
          message("Submitted ", count_text(submitted_now, "job"), " to STRING's values/ranks enrichment.")
        }
        # without waiting, the run ends after the submissions. A submission request that failed is
        # tried again, a few times as with waiting; an experiment that STRING cannot take now (e.g.
        # for the limit of active jobs) is left for a later submission.
        if (!wait) {
          if (!(request_failed && submitting)) {
            break
          }
          sleep(interval)
          interval <- min(VALUES_RANKS_LONGEST_POLL_INTERVAL, interval * 1.5)
          next
        }
      }

      waiting_for <- unfinished()
      if (length(waiting_for) == 0 && (length(pending) == 0 || !submitting)) {
        break
      }

      if (length(waiting_for) > 0) {
        states <- tryCatch(
          get_values_ranks_jobs_states(stable_url, key_info$key, send),
          error = function(e) e
        )

        if (inherits(states, "values_ranks_refused")) {
          stop_early(sub("^ERROR: ", "", conditionMessage(states)))
        }
        if (inherits(states, "error")) {
          if (is_interrupted_request(states)) {
            stop_early("The wait for the values/ranks jobs was interrupted.")
          }
          failed_status_requests <- failed_status_requests + 1
          if (failed_status_requests >= VALUES_RANKS_MAX_FAILED_STATUS_REQUESTS) {
            stop_early(paste(
              "The states of the values/ranks jobs could not be read from STRING (", conditionMessage(states), ").",
              sep = ""
            ))
          }
          waiting_for <- character(0)
        } else {
          failed_status_requests <- 0
        }

        state_positions <- match(unname(job_ids[waiting_for]), names(states))
        for (job_number in seq_along(waiting_for)) {
          experiment_name <- waiting_for[job_number]
          job_id <- job_ids[[experiment_name]]

          if (is.na(state_positions[job_number])) {
            # not listed for the key: a request for the job alone tells why; a request that fails
            # is asked again in the next round
            job <- tryCatch(
              get_values_ranks_job_status(stable_url, key_info$key, job_id, send),
              error = function(e) e
            )
            if (inherits(job, "values_ranks_refused")) {
              failed[experiment_name] <- sub("^ERROR: ", "", conditionMessage(job))
              next
            }
            if (inherits(job, "error")) {
              if (is_interrupted_request(job)) {
                stop_early("The wait for the values/ranks jobs was interrupted.")
              }
              next
            }
          } else {
            job <- states[[state_positions[job_number]]]
          }

          job_state <- values_ranks_state(job$status)
          if (job_state %in% VALUES_RANKS_FINISHED_STATES) {
            downloaded <- tryCatch(
              download_values_ranks_results(job, job_id, fetch),
              error = function(e) e
            )
            if (!inherits(downloaded, "error")) {
              results[[experiment_name]] <- downloaded
              next
            }
            if (is_interrupted_request(downloaded)) {
              stop_early("The download of the values/ranks results was interrupted.")
            }
            # a download that fails is tried again in the next rounds, a few times
            attempts <- 1
            if (!is.na(download_failures[experiment_name])) {
              attempts <- download_failures[[experiment_name]] + 1
            }
            download_failures[experiment_name] <- attempts
            if (attempts >= VALUES_RANKS_MAX_FAILED_STATUS_REQUESTS) {
              failed[experiment_name] <- sub("^ERROR: ", "", conditionMessage(downloaded))
            }
          } else if (job_state %in% VALUES_RANKS_FAILED_STATES) {
            failed[experiment_name] <- paste(
              "STRING's values/ranks job ", job_id, " failed: ", values_ranks_error_message(job$message),
              sep = ""
            )
          } else if (job_state == "expired") {
            failed[experiment_name] <- paste("STRING no longer has values/ranks job ", job_id, ".", sep = "")
          }
        }
      }

      # a progress message whenever the counts change
      n_waiting <- length(unfinished())
      report <- paste(length(results), length(failed), n_waiting, length(pending))
      if (!quiet && report != last_report) {
        not_submitted <- ""
        if (length(pending) > 0) {
          not_submitted <- paste(", ", length(pending), " not submitted yet (", held_back, ")", sep = "")
        }
        message(
          "STRING's values/ranks jobs: ", length(results), " finished, ", length(failed), " failed, ",
          n_waiting, " waiting or running", not_submitted, "."
        )
        last_report <- report
      }

      if (n_waiting == 0 && (length(pending) == 0 || !submitting)) {
        break
      }

      # the timeout is per job: a job that has not finished timeout_seconds after its submission is
      # not waited for any longer (it keeps running on STRING), and experiments that STRING could
      # not take for as long are not submitted. The results so far are returned
      # (values_ranks_results_list).
      now <- clock()
      late_jobs <- Filter(function(experiment_name) now - submitted_at[[experiment_name]] >= timeout_seconds, unfinished())
      if (length(late_jobs) > 0) {
        given_up <- c(given_up, late_jobs)
        timed_out <- TRUE
      }
      if (submitting && length(pending) > 0 && !is.na(held_back_since) && now - held_back_since >= timeout_seconds) {
        submitting <- FALSE
        submission_timed_out <- TRUE
        timed_out <- TRUE
        # held_back is STRING's last refusal
        explanation <- values_ranks_refusal_explanation(
          paste("STRING did not take them within", timeout_seconds, "seconds"), held_back
        )
        held_back_text <- explanation$text
        held_back_advice <- explanation$advice
      }
      if (length(unfinished()) == 0 && (length(pending) == 0 || !submitting)) {
        break
      }

      sleep(interval)
      interval <- min(VALUES_RANKS_LONGEST_POLL_INTERVAL, interval * 1.5)
    },
    interrupt = function(condition) {
      if (length(job_ids) > 0) {
        message("\nThe submitted values/ranks jobs keep running on STRING. ", resume_hint())
      }
    }
  )

  return(list(
    job_ids = job_ids, results = results, failed = failed, pending = pending, held_back = held_back,
    held_back_text = held_back_text, held_back_advice = held_back_advice,
    timed_out = timed_out, submission_timed_out = submission_timed_out, timeout_seconds = timeout_seconds
  ))
}


# The column numbers of the experiments that were not submitted (pending, from a table of
# values), named by experiment: values[, not_submitted, drop = FALSE] are their values.
values_ranks_not_submitted <- function(pending) {
  column_numbers <- vapply(pending, function(experiment) as.integer(experiment$column_number), integer(1))
  names(column_numbers) <- names(pending)
  return(column_numbers)
}


# The results of several experiments or jobs (run_values_ranks_jobs) as a list named by
# experiment (or job), in the order of experiment_names, with NULL for an experiment that failed,
# was not finished when the timeout ended the wait, or was not submitted. The attribute status
# says which, for each entry: "success", "failed", "unfinished" or "not submitted", so that a NULL
# entry is not taken for results without terms. The job ids are the attribute job_ids, and the
# column numbers of the experiments that were not submitted, named by experiment, the attribute
# not_submitted. Warnings name the failed experiments with their reasons, and say how to get the
# results of the unfinished jobs and how to submit the others. what names the entries in the
# warnings.
values_ranks_results_list <- function(run, experiment_names, what = "experiments") {
  results <- vector("list", length(experiment_names))
  names(results) <- experiment_names
  for (experiment_name in names(run$results)) {
    results[[experiment_name]] <- run$results[[experiment_name]]
  }

  # an entry that is neither finished, failed nor left unsubmitted is a job that did not finish
  # before the timeout
  status <- rep("unfinished", length(experiment_names))
  names(status) <- experiment_names
  status[names(run$results)] <- "success"
  status[names(run$failed)] <- "failed"
  status[names(run$pending)] <- "not submitted"

  attr(results, "status") <- status
  attr(results, "job_ids") <- run$job_ids
  if (length(run$pending) > 0) {
    attr(results, "not_submitted") <- values_ranks_not_submitted(run$pending)
  }

  if (length(run$failed) > 0) {
    warning(
      values_ranks_experiments_warning(
        paste("values/ranks", what, "failed"), run$failed, paste("values/ranks", sub("s$", "", what), "failed")
      ),
      call. = FALSE
    )
  }

  n_unfinished <- length(setdiff(names(run$job_ids), c(names(run$results), names(run$failed))))
  if (isTRUE(run$timed_out) && n_unfinished > 0) {
    warning(
      n_unfinished, " of ", length(run$job_ids), " values/ranks jobs did not finish within ", run$timeout_seconds,
      " seconds each. Their entries are NULL. The jobs keep running on STRING: get their results later with ",
      "string_db$get_values_ranks_results() and the attribute job_ids of these results. STRING's servers may be ",
      "busy. ", VALUES_RANKS_SUPPORT_HINT,
      call. = FALSE
    )
  }

  if (length(run$pending) > 0) {
    advice <- ""
    if (nchar(run$held_back_advice) > 0) {
      advice <- paste("", run$held_back_advice)
    }
    warning(
      "Not submitted: ", length(run$pending), " of ", length(experiment_names), " ", what, ". ", run$held_back_text,
      " Their entries are NULL, and values[, attr(results, \"not_submitted\"), drop = FALSE] are their values.", advice,
      call. = FALSE
    )
  }
  return(results)
}
