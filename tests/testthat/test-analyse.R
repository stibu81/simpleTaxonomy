test_that("count_ranks summarises the full example taxonomy", {
  counts <- count_ranks(read_taxonomy(get_example_taxonomy_file()))

  expect_s3_class(counts, "tbl_df")
  expect_equal(
    as.character(counts$rank),
    c(
      "Ordnung", "Unterordnung", "Überfamilie", "Familie", "Unterfamilie",
      "Tribus", "Gattung", "Art", "Unterart", "ohne Rang"
    )
  )
  expect_equal(counts$n, c(1, 2, 1, 14, 2, 2, 13, 57, 3, 1))
})


test_that("count_ranks supports subgraphs, grouping, and major ranks", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  cats <- count_ranks(graph, subgraph = "Katzen")
  expect_equal(as.character(cats$rank), c("Familie", "Unterfamilie", "Gattung", "Art"))
  expect_equal(cats$n, c(1, 2, 3, 12))

  by_family <- suppressWarnings(count_ranks(graph, "Hundeartige", by_rank = "Familie"))
  expect_equal(names(by_family), c("Familie", "Tribus", "Gattung", "Art", "Unterart"))
  expect_equal(by_family$Familie[[1]], "Hunde")
  expect_equal(by_family$Art[by_family$Familie == "Bären"], 8)

  major <- count_ranks(graph, only_major_ranks = TRUE)
  expect_equal(as.character(major$rank), c("Ordnung", "Familie", "Gattung", "Art"))
  expect_equal(major$n, c(1, 14, 13, 57))
})


test_that("count_ranks validates its inputs", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  expect_error(count_ranks(list()), "not a taxonomy_graph")
  expect_error(count_ranks(graph, by_rank = c("Familie", "Art")), "length one")
  expect_error(count_ranks(graph, by_rank = "invalid"), "not a valid rank")
  expect_error(count_ranks(graph, by_rank = "ohne Rang"), "cannot be used")
})
