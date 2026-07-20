library(igraph)

taxonomy <- read_taxonomy(get_example_taxonomy_file())

test_that("print.taxonomy_graph() works with default values", {
  expect_snapshot(print(taxonomy))
})



test_that("print.taxonomy_graph() works with less ranks", {
  expect_snapshot(print(taxonomy, n_ranks = 3))
})


test_that("print.taxonomy_graph() works without rank summary", {
  expect_snapshot(print(taxonomy, n_ranks = 0))
})


test_that("print.taxonomy_graph() works if there are no image urls", {
  taxonomy_no_images <- set_vertex_attr(taxonomy, "image_url", value = NA_character_)
  expect_snapshot(print(taxonomy_no_images))
})