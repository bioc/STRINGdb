# STRINGdb 2.25.4

## New features

- `get_values_ranks_enrichment()` runs a GSEA-like gene set enrichment analysis of a whole ranked
  experiment, computed on STRING's servers, not locally: STRING's values/ranks enrichment ("Proteins
  with Values/Ranks" on the STRING website), on STRING's job queue. It submits the job, waits for it
  and returns the enriched terms as a data frame, with the direction of each term (top, bottom or both
  ends of the ranked list) and the job's addresses on the STRING website. It needs STRING 12.0 or
  later. STRING's statistics differ from the GSEA of Subramanian et al. (e.g. fgsea or
  clusterProfiler); its help explains how. The values can also be given alone, as R's GSEA packages
  take them: a numeric vector named by STRING identifiers, or a table with STRING identifiers as row
  names. Infinite values (e.g. -log10 of a p-value of 0) are left out with a warning. The results
  have the columns of `get_enrichment()` where they mean the same (`term`, `number_of_genes`,
  `inputGenes`, `fdr`, `description`, ...) and its category codes (`"Process"`, `"RCTM"`, ...); the
  values and ranks of each term's proteins are the list columns `inputValues` and `inputRanks`.
- Many experiments at once: with a table of values (one column for each experiment),
  `get_values_ranks_enrichment()` submits one job for each experiment, which STRING runs side by side
  on its servers, waits for all and returns a list of results. Up to 1000 jobs of an API key may wait
  or run at once; the method submits the others as jobs finish. To submit without waiting,
  `submit_values_ranks_enrichment()` returns the job identifiers at once, `get_values_ranks_status()`
  shows the state of the jobs (`"submitted"`, `"running"`, `"success"`, `"failed"` or `"unknown"`;
  without job identifiers, of all jobs of the API key), and `get_values_ranks_results()` returns
  their results later, also in a new R session. A job that fails does not stop the others, and when
  the timeout ends the wait, the results so far are returned, with the job identifiers to get the
  rest later; the attribute `status` of a list of results gives the state of each experiment
  (`"success"`, `"failed"`, `"unfinished"` or `"not submitted"`).
- The values/ranks methods need a STRING API key, which the package requests the first time and keeps
  on the computer (in `tools::R_user_dir("STRINGdb", "config")`); such a key works at once.
  `STRINGDB_API_KEY`, when set, is used instead, e.g. to share one key across the nodes of a cluster,
  or to read the jobs of another key; the methods take no key as an argument. `get_api_key()` shows
  the key of the computer, and `get_api_key(new = TRUE)` replaces it.
- `get_enrichment(add_scores = TRUE)` adds STRING's strength and signal columns (STRING 12.5).
- `get_enrichment_figure()` accepts the categories GWAS and Hallmark, the palette `"red_blue"` and
  `x_axis = "count"` (STRING 12.5). With an older version these values stop with an error instead of
  silently giving STRING's default figure. `sort_by` also accepts `"count"`; like `graph_type` and the
  filters, `sort_by` is not used with an older version, with a warning.
- `get_png()`, `get_link()` and `plot_network()`:
  - `channels` selects the evidence channels (e.g. `c("experiments", "databases")`, or by the column
    names of the detailed interaction tables, e.g. `"experimental"`) from which STRING computes the
    combined score (STRING 12.0 or later);
  - `add_color_nodes` and `add_white_nodes` add interaction partners to the network;
  - `show_query_node_labels` labels the proteins with the identifiers given.

## Bug fixes

- A download of a STRING file that is cut off part-way, for example by a timeout, no longer leaves the
  part that arrived in the download cache (`input_directory`), where later calls took it for the whole
  file. `downloadAbsentFile()` saves a download under a temporary name and gives it the file's name only
  when it has finished. A part left by an older version in a kept `input_directory` is not detected;
  delete it to download the file again.
- A download of a STRING file may take up to 300 seconds instead of R's default timeout of 60 seconds,
  which cut off the larger files on a slower connection. A higher `options(timeout)` is kept.
- The note of `map()` about identifiers that could not be mapped ends with a line break, so that the
  next output no longer continues on its line.

## Other changes

- The package needs R 4.0.0 or later (`Depends: R (>= 4.0.0)`), for `tools::R_user_dir()`, where it
  keeps the API key.
- The package itself sends its caller identity, `"STRINGdb-package"`, with every request to STRING's
  API and every download of STRING's data files. The `caller_identity` argument of `get_png()`, `get_link()`, `get_enrichment_figure()` and
  `get_geneset_description()` is now ignored, with the default `NULL`, so that code that gives it
  still runs, with a warning (once per R session). It will be removed in the next Bioconductor
  release, so do not specify it.
- Several methods have new arguments. A call that abbreviates an argument name may need the full name,
  e.g. `show =` for `show_regulatory_signs` of `get_link()` and `plot_network()`.
- igraph's deprecated function names are replaced by their current names.
- The unit tests that call STRING are deactivated instead of failing when STRING cannot be reached.

## Planned for the next Bioconductor release

- The columns `inputGenes` and `preferredNames` of `get_enrichment()` and
  `get_values_ranks_enrichment()` will become list columns (a character vector for each term)
  instead of comma-separated text. Code that splits them with `strsplit()`, or writes the results
  with `write.csv()` or `write.table()`, will need to change: these take no list columns.
- The ignored `caller_identity` argument of `get_png()`, `get_link()`, `get_enrichment_figure()` and
  `get_geneset_description()` will be removed: a call that gives it will then stop with an "unused
  argument" error.

# STRINGdb 2.25.3

## Bug fixes

- `STRINGdb$new()` creates an object for a known STRING version (11.0, 11.0b, 11.5, 12.0 and 12.5)
  without any request to STRING, so it also works offline or when string-db.org cannot be reached.
  Without a version, STRING is asked for its current version with a timeout, and the newest known
  version is used, with a warning, when STRING cannot be reached. An unavailable version now stops with
  an error that names the available versions.
- `map()` no longer gives wrong results in three cases: missing (NA) or empty identifiers were mapped to
  a real protein (for human, every row without a gene name got `9606.ENSP00000222345`); with several
  identifier columns, the identifiers no longer matched the rest of their rows; and with
  `takeFirst = FALSE` the identifiers came back upper-cased. Aliases containing characters such as the
  hyphen U+2010 can now be mapped. For other input the output is unchanged.
- `get_summary(string_ids)` works without `required_score`, as documented.
- `post_payload()` with `logo_imgF` or `legend_imgF` works again.

# STRINGdb 2.25.2

- `get_png()` and `get_enrichment_figure()` stop with STRING's explanation when STRING refuses the request,
  for example a PNG of more than 150,000 interactions, instead of returning STRING's error answer as the
  picture.
- Requests that STRING's servers answer with an error (HTTP 5xx, e.g. a gateway timeout) stop with the
  status code instead of returning the error page as data.
- New options for STRING 12.5 (older versions do not use them, with a warning):
  - `get_enrichment()` and `get_enrichment_figure()`: `max_fdr`, `min_gene_count`, `min_strength`,
    `min_signal` and `merge_similarity` filter the terms as the enrichment table of the STRING website;
  - `get_enrichment_figure()`: `graph_type` (`"dotplot"` or `"barplot"`) and `sort_by`;
  - `get_png()`: clustering of the network picture (`network_clustering_algorithm` `"kmeans"`, `"mcl"` or
    `"leiden"`, and its options), as the Clusters tab of the STRING website.

# STRINGdb 2.25.1

- Support for STRING 12.5, including its directed regulatory network (`network_type = "regulatory"`).
- New method `get_geneset_description()`, and more options for network pictures and links.
