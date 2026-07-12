test_that("print.taxonomy_graph shows a readable summary", {
  out <- paste(capture.output(print(read_taxonomy(get_example_taxonomy_file()), n_ranks = 2)), collapse = "\n")

  expect_match(out, "taxonomy_graph with 96 nodes.", fixed = TRUE)
  expect_match(out, "root node: Raubtiere", fixed = TRUE)
  expect_match(out, "tree depth: 6", fixed = TRUE)
  expect_match(out, "most common ranks:", fixed = TRUE)
  expect_match(out, "Art", fixed = TRUE)
})


test_that("print.taxonomy_graph returns the graph invisibly", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  invisible(capture.output(result <- print(graph, n_ranks = 0)))
  expect_identical(result, graph)
})
