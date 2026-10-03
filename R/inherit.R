#' Inherit Meta Data for Extinct, Local, and Observed in a Taxonomy File
#' 
#' A taxonomy file can optionally contain some or all of the logical columns
#' "extinct", "local", and "observed". The value that is assigned to some nodes
#' can determine the value of other nodes. As an example, if a species is marked
#' as local this implies that also the genus, family, etc. that the species
#' belongs to are local.
#' 
#' @param file path to the csv file
#' @param delim the delimiter used in the file
#' 
#' @details
#' Inheritance is only applied to columns that are already present. No new columns
#' are added.
#' 
#' For the columns "local" and "observed", inheritance works upwards: if a taxon
#' is local or has been observed, the same applies to all it's parents. Note that
#' only the value set for leaf taxa is taken into account.
#' 
#' For the columns "extinct", inheritance works downwards: if a taxon is extinct,
#' so are all taxa below it.
#' 
#' @return
#' a `taxonomy_graph` with modified columns "local", "observed", and "rank" (if
#' present). The file given by `file` is overwritten as a side effect.
#' 
#' @export

inherit_meta_data <- function(file, delim = ",") {

  taxonomy <- read_taxonomy(file, delim)

  # there are two modes of inheritance:
  # up: a value of TRUE is inherited to all parents (local, observed)
  # down: a value of TRUE is inherited to all children (extinct)
  if ("local" %in% igraph::vertex_attr_names(taxonomy)) {
    taxonomy <- inherit_up(taxonomy, "local")
  }
  if ("observed" %in% igraph::vertex_attr_names(taxonomy)) {
    taxonomy <- inherit_up(taxonomy, "observed")
  }
  if ("observed" %in% igraph::vertex_attr_names(taxonomy)) {
    taxonomy <- inherit_down(taxonomy, "extinct")
  }

  write_taxonomy_csv(taxonomy, file, delim)

  taxonomy
}


inherit_up <- function(graph, attr) {
  # only the value set for leaf-nodes matters => set all others to NA
  leafs <- get_leaf_nodes(graph)
  igraph::vertex_attr(graph, attr, -leafs) <- NA

  # get the leafs where the attribute is true. Every node on the path
  # to the root must then also be set to true.
  true_leaves <- leafs[which(igraph::vertex_attr(graph, attr, leafs))]
  root <- get_root_node(graph)
  true_paths <- igraph::shortest_paths(graph, from = root, to = true_leaves)
  true_nodes <- true_paths$vpath %>% 
    unlist() %>% 
    unique()
  igraph::vertex_attr(graph, attr, true_nodes) <- TRUE

  graph
}


inherit_down <- function(graph, attr) {
  # only non_leaf_nodes that are TRUE must be inherited
  nleafs <- igraph::V(graph)[-get_leaf_nodes(graph)]
  true_non_leaves <- nleafs[which(igraph::vertex_attr(graph, attr, nleafs))]

  # get all the nodes below true non-leaves
  true_nodes <- lapply(true_non_leaves, \(v) igraph::subcomponent(graph, v, "out")) %>% 
    unlist() %>% 
    unique()
  igraph::vertex_attr(graph, attr, true_nodes) <- TRUE

  graph
} 