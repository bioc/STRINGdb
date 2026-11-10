# STRINGdb 2.25.5

## Other changes

- Vignette: the GSEA code is plain code in the vignette's R script (it was commented out there), so
  it can be run, e.g. in RStudio. It runs only in an interactive R session, not while the vignette
  is built.

# STRINGdb 2.25.4

## New features

- `get_values_ranks_enrichment()`: a GSEA-like enrichment analysis of a ranked list of proteins,
  computed on STRING's servers (STRING's values/ranks enrichment; STRING 12.0 or later). The results
  have the columns of `get_enrichment()` where they mean the same; STRING's statistics differ from
  those of fgsea or clusterProfiler. The values can also be given alone, as a named vector.
- Many experiments at once: with a table of values, each experiment becomes a job, run side by side
  on STRING's servers (up to 1000 jobs per API key). For many experiments, e.g. tens or hundreds of
  columns, `submit_values_ranks_enrichment()` submits them without blocking the R session;
  `get_values_ranks_status()` and `get_values_ranks_results()` follow them and collect the results
  later, also in a new R session.
- `get_api_key()`: the values/ranks methods use one API key per computer, which the package requests
  and saves; `STRINGDB_API_KEY` is used instead when it is set.
- `get_enrichment(add_scores = TRUE)` adds STRING's strength and signal columns (STRING 12.5).
- `get_enrichment_figure()`: the categories GWAS and Hallmark, the palette `"red_blue"`, and
  `"count"` for `x_axis` and `sort_by` (STRING 12.5).
- `get_png()`, `get_link()` and `plot_network()`: `channels` selects the evidence channels (STRING
  12.0 or later), and `add_color_nodes` and `add_white_nodes` add interaction partners.

## Bug fixes

- A download cut off part-way no longer leaves a partial file in the download cache; delete such a
  file left by an older version to download it again.
- Downloads of STRING files may take up to 300 seconds instead of R's default 60.
- `map()`'s note about unmapped identifiers ends with a line break.

## Other changes

- Depends on R 4.0.0 or later (for `tools::R_user_dir()`).
- The package sends its caller identity, `"STRINGdb-package"`, with every request to STRING. The
  `caller_identity` argument of `get_png()`, `get_link()`, `get_enrichment_figure()` and
  `get_geneset_description()` is ignored, with a warning.
- New arguments can make abbreviated argument names ambiguous, e.g. `show =` in `get_link()` and
  `plot_network()`: use the full names.
- igraph's deprecated function names are replaced by their current names.
- The unit tests that call STRING are deactivated when STRING cannot be reached.

## Planned for the next Bioconductor release

- `inputGenes` and `preferredNames` of `get_enrichment()` and `get_values_ranks_enrichment()` will
  become list columns instead of comma-separated text (code that splits them with `strsplit()` will
  need to change).
- The `caller_identity` argument will be removed.

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
  `takeFirst = FALSE` or `usePreferredSources = FALSE` some identifiers came back upper-cased. Aliases
  containing characters such as the hyphen U+2010 can now be mapped, and `get_aliases()` includes them.
  The identifier columns keep their type (e.g. integer Entrez ids are no longer turned into text), and
  with several identifier columns the rows keep their input order.
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
