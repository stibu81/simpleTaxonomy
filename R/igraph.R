#' Get the Root Node and the Leaves of a Taxonomy Graph
#'
#' @param graph a `taxonomy_graph` object
#'
#' @return
#' a `igraph.vs` object corresponding to the root node (for
#' `get_root_node()`) or the leaf nodes (for `get_leaf_nodes()`)
#' of the graph.
#'
#' @examples
#' file <- get_example_taxonomy_file()
#' taxonomy <- read_taxonomy(file)
#' get_root_node(taxonomy)
#' get_leaf_nodes(taxonomy)
#'
#' @export

get_root_node <- function(graph) {
  igraph::V(graph)[igraph::degree(graph, mode = "in") == 0]
}


#' @rdname get_root_node
#' @export

get_leaf_nodes <- function(graph) {
  igraph::V(graph)[igraph::degree(graph, mode = "out") == 0]
}


#' Get Depth of a Taxonomy Tree Graph
#'
#' Get the depth of a taxonomy tree graph, i.e., the number of nodes from the
#' root to the most distant leaf.
#'
#' @param graph a `taxonomy_graph` object
#'
#' @return
#' integer giving the depth of the graph
#'
#' @examples
#' file <- get_example_taxonomy_file()
#' taxonomy <- read_taxonomy(file)
#' get_tree_depth(taxonomy)
#'
#' @export

get_tree_depth <- function(graph) {
  root <- get_root_node(graph)
  as.integer(max(igraph::dfs(graph, root, dist = TRUE)$dist) + 1)
}


# get the deepest nodes in the tree
get_deepest_nodes <- function(graph) {
  root <- get_root_node(graph)
  distances <- igraph::dfs(graph, root, dist = TRUE)$dist
  igraph::V(graph)[distances == max(distances)]
}


#' Extract a Subgraph from a Taxonomy Graph
#'
#' Extract a subgraph from a `taxonomy_graph` below a given taxon.
#'
#' @param graph a `taxonomy_graph` object
#' @param taxon character giving a single taxon. The subgraph will contain
#' this taxon as the root and all the taxa that are in the tree below it.
#'
#' @return
#' a `taxonomy_graph` object
#'
#' @examples
#' file <- get_example_taxonomy_file()
#' taxonomy <- read_taxonomy(file)
#' cats <- get_subgraph(taxonomy, "Katzen")
#'
#' @export

get_subgraph <- function(graph, taxon) {

  # this only works for taxonomy_graph objects
  if (!inherits(graph, "taxonomy_graph")) {
    cli::cli_abort(
      "{deparse(substitute(graph))} is not a taxonomy_graph object."
    )
  }

  # check taxon: there must be only one and it must be valid
  if (length(taxon) != 1) {
    cli::cli_abort("taxon must have length 1.")
  }
  if (!taxon %in% names(igraph::V(graph))) {
    cli::cli_abort("taxon \"{taxon}\" does not exist in the graph.")
  }

  subcomponent <- igraph::subcomponent(graph, taxon, mode = "out")
  subgraph <- igraph::subgraph(graph, subcomponent)
  class(subgraph) <- c("taxonomy_graph", class(subgraph))

  subgraph
}

# Create a taxonomy_graph from the data read from csv
create_taxonomy_graph <- function(data, error_call = rlang::caller_env()) {

  # all the additional columns are vertex attributes
  vertex_attr <- data %>%
    dplyr::select(-"parent")

  graph <- data %>%
    dplyr::select("parent", "name") %>%
    dplyr::filter(!is.na(.data$parent)) %>%
    igraph::graph_from_data_frame(vertices = vertex_attr)

  # if the conditions checked in check_taxonomy_df are satisfied (no duplicate
  # names), the graph should always be a tree. But just to be sure, we still
  # check.
  if (!igraph::is_tree(graph)) {
    cli::cli_abort("The graph is not a tree.", call = error_call)
  }

  class(graph) <- c("taxonomy_graph", class(graph))

  graph
}


#' @importFrom dplyr as_tibble
#' @export
dplyr::as_tibble

#' @export
as_tibble.taxonomy_graph <- function(x, ...) {

  edges <- igraph::as_data_frame(x, "edges") %>%
    dplyr::as_tibble() %>%
    dplyr::rename("parent" = "from", "name" = "to") %>%
    # add a fake edge for the root node at the top of the table to get
    # the original format that was used to create the graph
    dplyr::add_row(
      parent = NA_character_,
      name = names(get_root_node(x)),
      .before = 1
    )

  vertices <- igraph::as_data_frame(x, "vertices") %>%
    dplyr::as_tibble()

  # if column image_url is completely empty, remove it
  if (all(is.na(vertices$image_url))) {
    vertices <- vertices %>% dplyr::select(-"image_url")
  }

  edges %>%
    dplyr::full_join(vertices, by = "name", relationship = "one-to-one") %>%
    dplyr::select(-dplyr::any_of(c("label", "colour", "collapsed")))

}


# convert a graph to a nested list in the format required by the htmlwidget
# from callapsibleTree
graph_as_nested_list <- function(graph) {

  # igraph::neighbors runs much faster, if integer indices are used instead
  # of igraph.vs objects. Use this setting to ensure that this happens
  opt_old <- igraph::igraph_options(return.vs.es = FALSE)
  on.exit(igraph::igraph_options(opt_old))

  # extract vertex attributes once to avoide repeated extraction in
  # graph_as_nested_list_recursive()
  vertices <- igraph::vertex_attr(graph)

  # commpute children only once to avoid repeated computation in
  # graph_as_nested_list_recursive()
  all_children <- igraph::adjacent_vertices(graph, igraph::V(graph))

  # do the recursion
  graph_as_nested_list_recursive(
    graph,
    as.integer(get_root_node(graph)),
    vertices,
    all_children
  )
}


graph_as_nested_list_recursive <- function(graph,
                                           root,
                                           vertices,
                                           all_children) {

  node_list <- list(
    name = vertices$label[root],
    collapsed = vertices$collapsed[root],
    fill = vertices$colour[root],
    tooltip = vertices$tooltip[root]
  )

  # if there are children, they must be adde to the list
  # note that using mode with an integer is much faster
  children <- all_children[[root]]
  if (length(children) == 0) return(node_list)

  c(node_list,
    list(
      children = unname(
        lapply(children, graph_as_nested_list_recursive,
               graph = graph, vertices = vertices, all_children = all_children)
      )
    )
  )
}
