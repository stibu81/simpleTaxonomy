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

