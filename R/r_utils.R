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
    text <- gsub("<[^>]*>", " ", text)
    text <- gsub("&quot;", '"', text, fixed = TRUE)
    text <- gsub("&#x27;", "'", text, fixed = TRUE)
    text <- gsub("&#39;", "'", text, fixed = TRUE)
    text <- gsub("&lt;", "<", text, fixed = TRUE)
    text <- gsub("&gt;", ">", text, fixed = TRUE)
    text <- gsub("&nbsp;", " ", text, fixed = TRUE)
    text <- gsub("&amp;", "&", text, fixed = TRUE)
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


# .stop_on_client_error: also stop when STRING refused the request (HTTP status
# 4xx), with STRING's explanation (see stop_on_client_error)
postFormSmart <- function(uri, .params = list(), .opts = list(), .ctype = "text", .stop_on_client_error = FALSE) {
  res <- POST(url = uri, body = .params, config = .opts)
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
    GET(url_str, config = timeout(timeout_seconds), query = list(caller_identity = "STRINGdb-package")),
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


# The options that STRING's API has since version 12.5 (the enrichment filters, the enrichment figure's graph_type
# and sort_by, and the clustering of network pictures) are ignored without a word by older versions, such as
# version-12-0.string-db.org: a request with them would get unfiltered terms or a picture without clusters.
# Returns the options to send (those given), or none for an older version, with a warning when any was given.
options_supported_by_version <- function(params, version, options_description) {
  params <- drop_null_params(params)

  version_number <- suppressWarnings(as.numeric(version))
  version_supports_options <- !is.na(version_number) && version_number >= 12.5

  if (version_supports_options || length(params) == 0) {
    return(params)
  }

  warning(
    options_description, " need STRING version 12.5 or later; version ", version,
    " does not support them, so they are not used.",
    call. = FALSE
  )
  return(list())
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
downloadAbsentFile <- function(urlStr, oD = tempdir()) {
  fileName <- tail(strsplit(urlStr, "/")[[1]], 1)
  temp <- paste(oD, "/", fileName, sep = "")
  if (!file.exists(temp) || file.info(temp)$size == 0) download.file(urlStr, temp)
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
