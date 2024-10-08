#' Plot a Taxonomic Hierarchy
#'
#' Create an interactive visualisation of a taxonomic hierarchy.
#'
#' @param graph a `taxonomy_graph` object, typically created with
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
#' @param link_length length of the horizontal links that connect nodes
#' in pixels.
#' @param font_size font size of the labels in pixels.
#'
#' @return
#' a htmlwidget with the interactive tree visualisation.
#'
#' @export

plot_taxonomy <- function(graph,
                          show = c(),
                          expand_rank = c(),
                          full_expand = c(),
                          focus = c(),
                          link_length = 150,
                          font_size = 12) {

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

  expanded <- get_expanded(graph, show, expand_rank, full_expand)
  igraph::vertex_attr(graph, "collapsed") <- !expanded

  # for now, we must convert the graph back to a data frame for this to work
  data <- as_tibble(graph)

  collapsibleTree::collapsibleTreeNetwork(
    data,
    linkLength = link_length,
    fill = "colour",
    tooltipHtml = "tooltip",
    collapsed = "collapsed",
    fontSize = font_size
  )

}


# helper function to determine which nodes should be expanded
# (i.e., not collapsed)
get_expanded <- function(graph, show, expand_rank, full_expand) {

  # check that all the taxons in show and full_expand actually exist in the data.
  # Remove those that don't.
  show <- rm_invalid_taxons(show, graph)
  full_expand <- rm_invalid_taxons(full_expand, graph)

  # evaluate show: if it is not empty, find the path from all the required
  # taxons to the root. All nodes on the path must be expanded
  # (except for the starting taxons themselves)
  expanded_show <- if (length(show) == 0) {
    rep(FALSE, igraph::vcount(graph))
  } else {
    paths <- igraph::shortest_paths(graph, from = get_root_node(graph), to = show)
    expanded <- paths$vpath %>%
      lapply(\(x) utils::head(x, -1)) %>%
      # unlist() does not preserve the class, so we use do.call() instead
      do.call(what = c)
    igraph::V(graph) %in% expanded
  }

  # evaluate expand_full: if it is not empty, find the trees below the given
  # taxons and expand every taxon inside them.
  expanded_full <- if (length(full_expand) == 0) {
    rep(FALSE, igraph::vcount(graph))
  } else {
    subcomps <- lapply(
      full_expand,
      \(x) igraph::subcomponent(graph, x, mode = "out")
    )
    # unlist() does not preserve the class, so we use do.call() instead
    expanded <- do.call(c, subcomps)
    igraph::V(graph) %in% expanded
  }

  expanded_rank <- igraph::vertex_attr(graph, "rank") %in% expand_rank

  expanded_show | expanded_full | expanded_rank

}


# remove invalid taxons from character vector
rm_invalid_taxons <- function(x, graph) {

  bad_names <- setdiff(x, names(igraph::V(graph)))
  if (length(bad_names) > 0) {
    cli::cli_alert_danger(
      paste("The following taxons in \"{deparse(substitute(x))}\" do not exist",
            "and will be ignored:",
            "\"{paste(bad_names, collapse = '\", \"')}\"")
    )
  }

  setdiff(x, bad_names)
}
