test_that("igraph helpers expose taxonomy structure", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  expect_equal(names(get_root_node(graph)), "Raubtiere")
  expect_length(get_leaf_nodes(graph), 63)
  expect_equal(get_tree_depth(graph), 6)

  deepest <- names(simpleTaxonomy:::get_deepest_nodes(graph))
  expect_true(all(c("Europäischer Braunbär", "Grizzlybär", "Kodiakbär") %in% deepest))
})


test_that("subgraphs and tibbles preserve the relevant taxonomy data", {
  cats <- get_subgraph(read_taxonomy(get_example_taxonomy_file()), "Katzen")

  expect_s3_class(cats, "taxonomy_graph")
  expect_equal(igraph::vcount(cats), 18)
  expect_equal(names(get_root_node(cats)), "Katzen")

  cats_tbl <- as_tibble(cats)
  expect_equal(nrow(cats_tbl), 18)
  expect_true(all(c("parent", "name", "scientific", "rank", "image_url") %in% names(cats_tbl)))
  expect_false(any(c("label", "colour", "collapsed") %in% names(cats_tbl)))
})


test_that("internal graph builders create nested trees and reject non-trees", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  nested <- simpleTaxonomy:::graph_as_nested_list(graph)
  expect_equal(nested$name, "Raubtiere")
  expect_true(length(nested$children) > 0)

  cycle_data <- dplyr::tibble(
    parent = c("A", "B", "C"),
    name = c("B", "C", "A"),
    scientific = c("b", "c", "a"),
    rank = c("Art", "Art", "Art"),
    image_url = NA_character_,
    label = c("B", "C", "A"),
    colour = c("#A52A2A", "#A52A2A", "#A52A2A"),
    collapsed = c(TRUE, TRUE, TRUE)
  )
  expect_error(simpleTaxonomy:::create_taxonomy_graph(cycle_data), "not a tree")

  expect_type(simpleTaxonomy:::has_igraph_bug(), "logical")
  expect_length(simpleTaxonomy:::has_igraph_bug(), 1)
})
