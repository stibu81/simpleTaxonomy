# print taxonomy graph
#' @export
print.taxonomy_graph <- function(x, n_ranks = 5, ...) {

  cat("taxonomy_graph with", length(x), "nodes.\n")
  cat("root node:", names(get_root_node(x)), "\n")
  cat("tree depth:", get_tree_depth(x), "\n")
  cat("number of leaves:", length(get_leaf_nodes(x)), "\n")

  image_urls <- igraph::vertex_attr(x, "image_url")
  if (any(!is.na(image_urls))) {
    cat("graph has image url for",
        sum(!is.na(image_urls) & image_urls != "not_found"),
        "nodes.")
  } else {
    cat("graph has no image urls.")
  }

  if (n_ranks > 0) {
    cat("\n\nmost common ranks:\n")
    common_ranks <- count_ranks(x) %>%
      dplyr::slice_max(order_by = .data$n, n = n_ranks)
    # use data frame printing method, which is more appropriate here
    print.data.frame(common_ranks, right = FALSE, row.names = FALSE)
  }

  invisible(x)
}
