test_normalize_evidence_channels <- function() {

  # Without channels nothing is sent: STRING uses all channels.
  checkTrue(identical(STRINGdb:::normalize_evidence_channels(), list()))
  checkTrue(identical(STRINGdb:::normalize_evidence_channels(NULL), list()))

  # All seven channel parameters are sent, in order, "on" for the selected channels and "off" for the others
  # (STRING turns off the channels that are not sent).
  textmining_only <- STRINGdb:::normalize_evidence_channels("textmining")
  checkEquals(names(textmining_only), paste("channel", 1:7, sep = ""))
  checkEquals(unlist(textmining_only, use.names = FALSE), c(rep("off", 6), "on"))

  # Names in any case, given more than once, count once.
  selected <- STRINGdb:::normalize_evidence_channels(c("Experiments", "databases", "experiments"))
  checkEquals(unlist(selected, use.names = FALSE), c("off", "off", "off", "off", "on", "on", "off"))

  # The column names of the interaction tables with link_data = "detailed" stand for their channels.
  checkEquals(
    STRINGdb:::normalize_evidence_channels(c("experiments", "databases", "cooccurrence")),
    STRINGdb:::normalize_evidence_channels(c("experimental", "Database", "cooccurence"))
  )

  # The channel numbers of STRING's API.
  all_channels <- c("neighborhood", "fusion", "cooccurrence", "coexpression", "experiments", "databases", "textmining")
  for (channel_number in 1:7) {
    one_channel <- STRINGdb:::normalize_evidence_channels(all_channels[channel_number])
    checkEquals(which(unlist(one_channel, use.names = FALSE) == "on"), channel_number)
  }
  checkTrue(all(unlist(STRINGdb:::normalize_evidence_channels(all_channels)) == "on"))

  # Unknown names, an empty selection, NA and values that are not names stop with an error.
  error_message <- function(expression) {
    tryCatch(
      {
        force(expression)
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
  unknown_message <- error_message(STRINGdb:::normalize_evidence_channels(c("textmining", "homology")))
  checkTrue(!is.null(unknown_message))
  checkTrue(grepl("unknown channels: 'homology'", unknown_message, fixed = TRUE))
  checkTrue(grepl("'neighborhood'", unknown_message, fixed = TRUE))

  checkTrue(!is.null(error_message(STRINGdb:::normalize_evidence_channels(character(0)))))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_evidence_channels(c("textmining", NA)))))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_evidence_channels(""))))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_evidence_channels(5))))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_evidence_channels(TRUE))))

}


test_normalize_api_count <- function() {

  checkTrue(is.null(STRINGdb:::normalize_api_count(NULL, "add_white_nodes")))
  checkTrue(identical(STRINGdb:::normalize_api_count(0, "add_white_nodes"), 0L))
  checkTrue(identical(STRINGdb:::normalize_api_count(10, "add_white_nodes"), 10L))
  checkTrue(identical(STRINGdb:::normalize_api_count(5L, "add_color_nodes"), 5L))

  # Numbers that are not whole numbers of at least 0 stop with an error that names the parameter.
  error_message <- function(expression) {
    tryCatch(
      {
        force(expression)
        NULL
      },
      error = function(e) conditionMessage(e)
    )
  }
  negative_message <- error_message(STRINGdb:::normalize_api_count(-1, "add_color_nodes"))
  checkTrue(grepl("add_color_nodes should be a whole number of at least 0", negative_message, fixed = TRUE))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_api_count(2.5, "add_color_nodes"))))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_api_count("3", "add_color_nodes"))))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_api_count(c(1, 2), "add_color_nodes"))))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_api_count(NA, "add_color_nodes"))))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_api_count(Inf, "add_color_nodes"))))
  checkTrue(!is.null(error_message(STRINGdb:::normalize_api_count(TRUE, "add_color_nodes"))))

}


test_evidence_channels_version <- function() {

  # Notes the warning of a call, and muffles it.
  call_and_note_warning <- function(expression) {
    warning_message <- NULL
    value <- withCallingHandlers(
      expression,
      warning = function(w) {
        warning_message <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    return(list(value = value, warning_message = warning_message))
  }

  channel_params <- STRINGdb:::normalize_evidence_channels("experiments")

  # Versions 12.0 and 12.5 recompute the scores from the selected channels: the channels are sent.
  for (supporting_version in c("12.5", "12.0")) {
    result <- call_and_note_warning(
      STRINGdb:::options_supported_by_version(channel_params, supporting_version, "The evidence channels", minimum_version = "12.0")
    )
    checkTrue(identical(result$value, channel_params))
    checkTrue(is.null(result$warning_message))
  }

  # Older versions ignore them without a word: they are not sent, with a warning.
  for (old_version in c("11.5", "11.0b", "11.0")) {
    result <- call_and_note_warning(
      STRINGdb:::options_supported_by_version(channel_params, old_version, "The evidence channels", minimum_version = "12.0")
    )
    checkTrue(length(result$value) == 0)
    checkTrue(grepl("need STRING version 12.0 or later", result$warning_message, fixed = TRUE))
  }

  # Without minimum_version, the options need version 12.5, with the same warning as before.
  result <- call_and_note_warning(STRINGdb:::options_supported_by_version(list(sort_by = "FDR"), "12.0", "sort_by"))
  checkTrue(length(result$value) == 0)
  checkEquals(result$warning_message, "sort_by need STRING version 12.5 or later; version 12.0 does not support them, so they are not used.")

}


test_network_picture_options_requests <- function() {

  # The requests of get_png, get_link and plot_network are captured instead of sent (no request reaches
  # STRING): postFormSmart is replaced for this test, and restored at its end.
  sent_requests <- list()
  tiny_png <- png::writePNG(array(1, c(2, 2, 3)))
  capture_request <- function(uri, .params = list(), .opts = list(), .ctype = "text", .stop_on_client_error = FALSE) {
    sent_requests[[length(sent_requests) + 1]] <<- list(uri = uri, params = .params)
    if (.ctype == "raw") {
      return(tiny_png)
    }
    if (grepl("get_link", uri, fixed = TRUE)) {
      return("https://string-db.org/cgi/link?to=TEST")
    }
    return("<svg></svg>")
  }
  original_post_form_smart <- get("postFormSmart", envir = asNamespace("STRINGdb"))
  assignInNamespace("postFormSmart", capture_request, ns = "STRINGdb")
  on.exit(assignInNamespace("postFormSmart", original_post_form_smart, ns = "STRINGdb"), add = TRUE)

  new_option_names <- c("add_color_nodes", "add_white_nodes", "show_query_node_labels", paste("channel", 1:7, sep = ""))
  string_ids <- c("9606.ENSP00000269305", "9606.ENSP00000258149")
  string_db <- STRINGdb$new(version = "12.5", species = 9606, score_threshold = 400)

  # Without the new options, none of their parameters is sent; get_link sends its limit of 0, as before.
  string_db$get_png(string_ids, output_format = "svg")
  string_db$get_link(string_ids)
  for (request in sent_requests) {
    checkTrue(!any(new_option_names %in% names(request$params)))
  }
  checkTrue(is.null(sent_requests[[1]]$params$limit))
  checkEquals(sent_requests[[2]]$params$limit, 0)

  # The options are sent as STRING's parameters.
  sent_requests <- list()
  string_db$get_png(
    string_ids, output_format = "svg", add_color_nodes = 5, add_white_nodes = 0,
    show_query_node_labels = TRUE, channels = c("experiments", "databases")
  )
  string_db$get_link(string_ids, add_white_nodes = 3, show_query_node_labels = 0, channels = "textmining")

  png_params <- sent_requests[[1]]$params
  checkTrue(grepl("/api/svg/network", sent_requests[[1]]$uri, fixed = TRUE))
  checkTrue(identical(png_params$add_color_nodes, 5L))
  checkTrue(identical(png_params$add_white_nodes, 0L))
  checkTrue(identical(png_params$show_query_node_labels, 1L))
  checkEquals(unlist(png_params[paste("channel", 1:7, sep = "")], use.names = FALSE), c("off", "off", "off", "off", "on", "on", "off"))

  link_params <- sent_requests[[2]]$params
  checkTrue(grepl("/get_link", sent_requests[[2]]$uri, fixed = TRUE))
  checkTrue(is.null(link_params$add_color_nodes))
  checkEquals(link_params$limit, 0)
  checkTrue(identical(link_params$add_white_nodes, 3L))
  checkTrue(identical(link_params$show_query_node_labels, 0L))
  checkEquals(unlist(link_params[paste("channel", 1:7, sep = "")], use.names = FALSE), c(rep("off", 6), "on"))

  # STRING's network page takes add_color_nodes as its limit: get_link sends it as the limit too, instead of 0.
  sent_requests <- list()
  string_db$get_link(string_ids, add_color_nodes = 4)
  checkTrue(identical(sent_requests[[1]]$params$add_color_nodes, 4L))
  checkTrue(identical(sent_requests[[1]]$params$limit, 4L))

  # plot_network passes the options on to get_png, and to get_link for add_link.
  sent_requests <- list()
  grDevices::pdf(NULL)
  string_db$plot_network(
    string_ids, add_link = TRUE, add_summary = FALSE, add_white_nodes = 2,
    show_query_node_labels = TRUE, channels = "coexpression"
  )
  grDevices::dev.off()
  checkEquals(length(sent_requests), 2)
  checkTrue(grepl("/api/image/network", sent_requests[[1]]$uri, fixed = TRUE))
  checkTrue(grepl("/get_link", sent_requests[[2]]$uri, fixed = TRUE))
  for (request in sent_requests) {
    checkTrue(identical(request$params$add_white_nodes, 2L))
    checkTrue(identical(request$params$show_query_node_labels, 1L))
    checkEquals(request$params$channel4, "on")
    checkEquals(request$params$channel7, "off")
  }

  # Invalid options stop before any request.
  sent_requests <- list()
  fails <- function(expression) {
    tryCatch(
      {
        force(expression)
        FALSE
      },
      error = function(e) TRUE
    )
  }
  checkTrue(fails(string_db$get_png(string_ids, channels = "homology")))
  checkTrue(fails(string_db$get_png(string_ids, channels = character(0))))
  checkTrue(fails(string_db$get_png(string_ids, add_color_nodes = -1)))
  checkTrue(fails(string_db$get_link(string_ids, add_white_nodes = 1.5)))
  checkTrue(fails(string_db$get_link(string_ids, show_query_node_labels = "yes")))
  checkEquals(length(sent_requests), 0)

  # Version 11.5 ignores the channels: they are not sent, with a warning; the other options are sent.
  old_string_db <- STRINGdb$new(version = "11.5", species = 9606, score_threshold = 400)
  warned <- FALSE
  withCallingHandlers(
    old_string_db$get_link(string_ids, add_color_nodes = 1, channels = "experiments"),
    warning = function(w) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  checkTrue(warned)
  old_params <- sent_requests[[1]]$params
  checkTrue(identical(old_params$add_color_nodes, 1L))
  checkTrue(!any(paste("channel", 1:7, sep = "") %in% names(old_params)))

}
