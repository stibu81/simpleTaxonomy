#' Plot a Taxonomic Hierarchy
#'
#' Create an interactive visualisation of a taxonomic hierarchy.
#'
#' @param data tibble defining the taxonomic hierarchy, typically created with
#' [`read_taxonomy()`].
#' @param show character giving the names of taxons that should be visible.
#' The tree will be shown uncollapsed up to those taxons.
#' @param expand_rank character giving the names of ranks that should always
#' be expanded.
#' @param font_size integer giving the font size of the labels in pixels.
#'
#' @return
#' a htmlwidget with the interactive tree visualisation.
#'
#' @export

plot_taxonomy <- function(data,
                          show = c(),
                          expand_rank = c(),
                          font_size = 12) {

  # add columns for colour
  colours <- c(
    "Lebewesen" = "#104E8B",
    "Dom\u00e4ne" = "#1C86EE",
    "Reich" = "#00FFFF",
    "Stamm" = "#76EEC6",
    "Unterstamm" = "#66CDAA",
    "Klasse" = "#00CC00",
    "Ordnung" = "#33FF33",
    "\u00dcberfamilie" = "#CCF90D",
    "Familie" = "#FFF700",
    "Unterfamilie" = "#FFC600",
    "Tribus" = "#FFA500",
    "Gattung" = "#CD8500",
    "Art"= "#A52A2A",
    "Unterart" = "#7c2020"
  )
  data$colour <- unname(colours[data$rank])

  # add tooltip
  data$tooltip <- paste0(
    data$rank, "</br>",
    "<strong>", data$name, "</strong></br>",
    "(", data$scientific, ")"
  )

  data$collapsed <- get_collapsed(data, show, expand_rank)

  collapsibleTree::collapsibleTreeNetwork(
    data,
    fill = "colour",
    tooltipHtml = "tooltip",
    collapsed = "collapsed",
    fontSize = font_size
  )

}



get_collapsed <- function(data, show, expand_rank) {

  # check that all the names in show actually exist in the data. Remove those
  # that don't.
  bad_names <- setdiff(show, data$name)
  if (length(bad_names) > 0) {
    cli::cli_alert_danger(
      paste("The following taxons do not exist and will be ignored:",
            "\"{paste(bad_names, collapse = '\", \"')}\"")
    )
    show <- setdiff(show, bad_names)
  }

  # if show is empty, everything must be collapsed
  # this is checked only after removing invalid taxons, since show may only
  # be empty after that step.
  collapsed_show <- if (length(show) == 0) {
    return(rep(TRUE, nrow(data)))
  } else {

    # create a graph and find the path from all the required taxons to the root.
    # all nodes on the path must be uncollapsed (except for the starting points)
    data$parent[is.na(data$parent)] <- "_ROOT"
    graph <- igraph::graph_from_data_frame(data)
    paths <- igraph::shortest_paths(graph, from = "_ROOT", to = show)
    not_collapsed <- paths$vpath %>%
      lapply(\(x) utils::head(names(x), -1)) %>%
      unlist()

    !data$name %in% not_collapsed
  }

  collapsed_show & (!data$rank %in% expand_rank)

}
