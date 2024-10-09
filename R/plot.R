#' Plot a Taxonomic Hierarchy
#'
#' Create an interactive visualisation of a taxonomic hierarchy.
#'
#' @param graph a `taxonomy_graph` object, typically created with
#' [read_taxonomy()].
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
#' @details
#' The function makes use of the html widget defined in [`collapsibleTree`],
#' but not of the function [collapsibleTreeNetwork()]
#' from that package that can produce the same plot from the same input data.
#' `plot_taxonomy()` is less flexible than the function from
#' `collapsibleTree` but also much faster.
#'
#' @return
#' a htmlwidget with the interactive tree visualisation.
#'
#' @examples
#' file <- get_example_taxonomy_file()
#' taxonomy <- read_taxonomy(file)
#' plot_taxonomy(taxonomy)
#'
#' # expand the full path to the Grizzly bear
#' plot_taxonomy(taxonomy, show = "Grizzlybär")
#'
#' # fully expand the cat family (Felidae)
#' # The cats are not visible, because the nodes above are not expanded.
#' plot_taxonomy(taxonomy, full_expand = "Katzen")
#'
#' # expand the path up to the family of the cats and everything below
#' plot_taxonomy(taxonomy, focus = "Katzen")
#'
#' @export

plot_taxonomy <- function(graph,
                          show = c(),
                          expand_rank = c(),
                          full_expand = c(),
                          focus = c(),
                          link_length = 150,
                          font_size = 12) {

  # this only works for taxonomy_graph objects
  if (!inherits(graph, "taxonomy_graph")) {
    cli::cli_abort(
      "{deparse(substitute(graph))} is not a taxonomy_graph object."
    )
  }

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

  widget_input = list(
    data = graph_as_nested_list(graph),
    options = get_widget_options(graph, link_length, font_size)
  )
  htmlwidgets::createWidget("collapsibleTree", widget_input)

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


get_widget_options <- function(graph, link_length, font_size) {

  # compute the margins as the number of characters of the label of the
  # root node (which is the left-most of all labels) and the maximal number of
  # characters of the labels at the deepest level (which are the right-most of
  # all labels).
  # This is inspired by the code in collapsibleTree::collapsibleTreeNetwork()
  # by Adeel Khan (https://github.com/AdeelK93).
  left_margin <- nchar(names(get_root_node(graph)))
  right_margin <- max(nchar(names(get_deepest_nodes(graph))))

  list(
    hierarchy = 1:get_tree_depth(graph),
    linkLength = link_length, fontSize = font_size,
    tooltip = TRUE, collapsed = "collapsed", zoomable = TRUE,
    margin = list(
      top = 20,
      bottom = 20,
      left = (left_margin * 0.6 * font_size) + 25,
      right = (right_margin * 0.6 * font_size) + 25
    )
  )
}
