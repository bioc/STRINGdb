#
#  copyright:   Andrea Franceschini
#          (Swiss Institute of Bioinformatics)
#           andrea.franceschini@isb-sib.ch
#


postFormSmart <- function(uri, .params = list(), .opts = list(), .ctype = "text") {
  res <- POST(url = uri, body = .params, config = .opts)
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


prioritize_aliases_by_source <- function(aliasDf) {
  if (nrow(aliasDf) == 0) {
    return(aliasDf)
  }

  rank_lookup <- seq_along(PREFERRED_ALIAS_SOURCES)
  names(rank_lookup) <- PREFERRED_ALIAS_SOURCES

  aliasDf$alias_normalized <- toupper(iconv(aliasDf$alias, "WINDOWS-1252", "UTF-8"))
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
multi_map_df <- function(dfToMap, dfMap, strColsFrom, strColFromDfMap, strColToDfMap, caseSensitive = FALSE) {
  tempMatr <- matrix(NA, length(strColsFrom), nrow(dfToMap))
  for (i in 1:length(strColsFrom)) {
    if (!caseSensitive) {
      tempMatr[i, ] <- as.vector(dfToMap[, strColsFrom[i]])
      dfToMap[, strColsFrom[i]] <- toupper(iconv(dfToMap[, strColsFrom[i]], "WINDOWS-1252", "UTF-8"))
    }
  }
  if (!caseSensitive) {
    dfMap[, strColFromDfMap] <- toupper(iconv(dfMap[, strColFromDfMap], "WINDOWS-1252", "UTF-8"))
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

  for (i in 1:length(strColsFrom)) {
    if (!caseSensitive && length(tempMatr[i, ]) == length(df2[, strColsFrom[i]])) df2[, strColsFrom[i]] <- tempMatr[i, ]
  }

  return(df2)
}



## Rename column data frame
renameColDf <- function(df, colOldName, colNewName) {
  if (!(colOldName %in% names(df))) print(paste("ERROR: We cannot find ", colOldName, " in the data frame.", sep = ""))
  names(df)[which(names(df) == colOldName)] <- colNewName
  return(df)
}
