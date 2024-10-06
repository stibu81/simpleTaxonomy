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
#' @param full_expand character giving the names of taxons that should be fully
#' expanded, i.e., all taxons below the given taxon should be visible. Note
#' that this does not expand the graph above the given taxon, such that the
#' expanded part may be invisible. Use `show` to expand the graph up to a
#' given taxon.
#' @param focus character giving one or several taxons to focus on. This means
#' that the tree up to this taxon will be expanded as well as the full
#' subtree below that taxon. It is equivalent to putting the same taxons in
#' `show` and `full_expand`. If `focus` is used, those other two arguments will
#' be ignored.
#' @param font_size integer giving the font size of the labels in pixels.
#'
#' @return
#' a htmlwidget with the interactive tree visualisation.
#'
#' @export

plot_taxonomy <- function(data,
                          show = c(),
                          expand_rank = c(),
                          full_expand = c(),
                          focus = c(),
                          font_size = 12) {

  # add columns for colour
  colours <- c(
    "Lebewesen" = "#104E8B",
    "Dom\u00e4ne" = "#1C86EE",
    "Reich" = "#00FFFF",
    "Stamm" = "#76EEC6",
    "Unterstamm" = "#66CDAA",
    "Klasse" = "#00CC00",
    "Unterklasse" = "#0AD60A",
    "Ordnung" = "#33FF33",
    "Unterordnung" = "#9EFB18",
    "\u00dcberfamilie" = "#CCF90D",
    "Familie" = "#FFF700",
    "Unterfamilie" = "#FFC600",
    "Tribus" = "#FFA500",
    "Gattung" = "#CD8500",
    "Art"= "#A52A2A",
    "Unterart" = "#7C2020",
    "ohne Rang" = "#FFFFFF"
  )
  data$colour <- unname(colours[data$rank])

  # add tooltip
  data$tooltip <- paste0(
    data$rank, "</br>",
    "<strong>", data$name, "</strong></br>",
    "(", data$scientific, ")"
  )

  # process argument focus: put the taxons in there into both, show and
  # full_expand. Also warn, if show or full_expand have been used together
  # with focus.
  if (length(focus) > 0) {
    if (length(c(show, full_expand)) > 0) {
      cli::cli_alert_warning(
        paste("focus has been used togehter with show and/or full_expand.",
              "show and full_expand will be ignored.")
      )
    }
    show <- full_expand <- focus
  }

  data$collapsed <- !get_expanded(data, show, expand_rank, full_expand)

  collapsibleTree::collapsibleTreeNetwork(
    data,
    fill = "colour",
    tooltipHtml = "tooltip",
    collapsed = "collapsed",
    fontSize = font_size
  )

}


# helper function to determine which nodes should be expanded
# (i.e., not collapsed)
get_expanded <- function(data, show, expand_rank, full_expand) {

  # check that all the taxons in show and full_expand actually exist in the data.
  # Remove those that don't.
  show <- rm_invalid_taxons(show, data)
  full_expand <- rm_invalid_taxons(full_expand, data)

  # prepare the graph in case it is needed. This is the case, if one of the
  # following arguments has been used: show, full_expand
  # this is checked only after removing invalid taxons, since the arguments
  # may only be empty after that step.
  graph <- if (length(c(show, full_expand)) > 0) {
    data$parent[is.na(data$parent)] <- "_ROOT"
    igraph::graph_from_data_frame(data)
  }

  # evaluate show: if it is not empty, find the path from all the required
  # taxons to the root. All nodes on the path must be expanded
  # (except for the starting taxons themselves)
  expanded_show <- if (length(show) == 0) {
    rep(FALSE, nrow(data))
  } else {
    paths <- igraph::shortest_paths(graph, from = "_ROOT", to = show)
    expanded <- paths$vpath %>%
      lapply(\(x) utils::head(names(x), -1)) %>%
      unlist()

    data$name %in% expanded
  }

  # evaluate expand_full: if it is not empty, find the trees below the given
  # taxons and expand every taxon inside them.
  expanded_full <- if (length(full_expand) == 0) {
    rep(FALSE, nrow(data))
  } else {
    subcomps <- lapply(
      full_expand,
      \(x) names(igraph::subcomponent(graph, x, mode = "out"))
    )
    expanded <- unlist(subcomps)

    data$name %in% expanded
  }

  expanded_show | expanded_full | data$rank %in% expand_rank

}


# remove invalid taxons from character vector
rm_invalid_taxons <- function(x, data) {

  bad_names <- setdiff(x, data$name)
  if (length(bad_names) > 0) {
    cli::cli_alert_danger(
      paste("The following taxons in \"{deparse(substitute(x))}\" do not exist",
            "and will be ignored:",
            "\"{paste(bad_names, collapse = '\", \"')}\"")
    )
  }

  setdiff(x, bad_names)
}
