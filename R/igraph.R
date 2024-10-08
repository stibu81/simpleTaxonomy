#' Get the Root Node of a Taxonomy Graph
#'
#' @param graph a `taxonomy_graph` object
#'
#' @return
#' a `igraph.vs` object corresponding to the root node of the graph.
#'
#' @export

get_root_node <- function(graph) {
  igraph::V(graph)[igraph::degree(graph, mode = "in") == 0]
}


#' Get Depth of a Taxonomy Tree Graph
#'
#' Get the depth of a taxonomy tree graph, i.e., the number of nodes from the root
#' to the most distant leaf.
#'
#' @param graph a `taxonomy_graph` object
#'
#' @return
#' integer giving the depth of the graph
#'
#' @export

get_tree_depth <- function(graph) {
  root <- get_root_node(graph)
  as.integer(max(igraph::dfs(graph, root, dist = TRUE)$dist) + 1)
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

  dplyr::full_join(edges, vertices, by = "name", relationship = "one-to-one")
}


# convert a graph to a nested list in the format required by the htmlwidget
# from callapsibleTree
graph_as_nested_list <- function(graph, root = get_root_node(graph)) {

  root_attr <- igraph::vertex_attr(graph, index = root)
  node_list <- list(
    name = root_attr$name,
    collapsed = root_attr$collapsed,
    fill = root_attr$colour,
    tooltip = root_attr$tooltip
  )

  # if there are children, they must be adde to the list
  children <- igraph::neighbors(graph, root)
  if (length(children) == 0) return(node_list)

  c(node_list,
    list(
      children = unname(lapply(children, graph_as_nested_list, graph = graph))
    )
  )
}
