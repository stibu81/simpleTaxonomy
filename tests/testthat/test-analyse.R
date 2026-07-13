library(dplyr, warn.conflicts = FALSE)

taxonomy <- read_taxonomy(get_example_taxonomy_file())

test_that("count_ranks() summarises the example taxonomy", {
  counts <- count_ranks(taxonomy)

  expect_s3_class(counts, "tbl_df")
  # check that rank is an ordered factor and that the columns is ordered, then convert
  # to character for the comparison to avoid test failure when new ranks are added.
  expect_s3_class(counts$rank, "ordered")
  expect_equal(counts$rank, sort(counts$rank, decreasing = TRUE))
  counts$rank <- as.character(counts$rank)

  expect_equal(
    counts,
    tibble(
      rank = c("Ordnung", "Unterordnung", "Überfamilie", "Familie", "Unterfamilie", 
               "Tribus", "Gattung", "Art", "Unterart", "ohne Rang"),
      n = c(1L, 2L, 1L, 14L, 2L, 2L, 13L, 57L, 3L, 1L)
    )
  )
})


test_that("count_ranks() summarises the example taxonomy with only major ranks", {
  counts <- count_ranks(taxonomy, only_major_ranks = TRUE)

  expect_s3_class(counts, "tbl_df")
  # check that rank is an ordered factor and that the columns is ordered, then convert
  # to character for the comparison to avoid test failure when new ranks are added.
  expect_s3_class(counts$rank, "ordered")
  expect_equal(counts$rank, sort(counts$rank, decreasing = TRUE))
  counts$rank <- as.character(counts$rank)

  expect_equal(
    counts,
    tibble(
      rank = c("Ordnung", "Familie", "Gattung", "Art"),
      n = c(1L, 14L, 13L, 57L)
    )
  )
})


test_that("count_ranks() summarises a subgraph of the example taxonomy", {
  counts <- count_ranks(taxonomy, subgraph = "Katzen")

  expect_s3_class(counts, "tbl_df")
  # check that rank is an ordered factor and that the columns is ordered, then convert
  # to character for the comparison to avoid test failure when new ranks are added.
  expect_s3_class(counts$rank, "ordered")
  expect_equal(counts$rank, sort(counts$rank, decreasing = TRUE))
  counts$rank <- as.character(counts$rank)

  expect_equal(
    counts,
    tibble(
      rank = c("Familie", "Unterfamilie", "Gattung", "Art"),
      n = c(1L, 2L, 3L, 12L)
    )
  )
})


test_that("count_ranks() summarises the example taxonomy by rank", {
  counts <- count_ranks(taxonomy, subgraph = "Marderverwandte", by_rank = "Familie")

  expect_s3_class(counts, "tbl_df")
  expect_equal(
    counts,
    tibble(
      Familie = c("Marder", "Skunks", "Ailuridae", "Kleinbären"),
      Gattung = c(3L, 0L, 1L, 1L),
      Art = c(10L, 1L, 0L, 1L)
    )
  )
})


test_that("count_ranks() aborts on invalid inputs", {
  expect_error(count_ranks(list()), "not a taxonomy_graph object")
  expect_error(
    count_ranks(taxonomy, by_rank = c("Familie", "Art")),
    "by_rank must have length one"
  )
  expect_error(
    count_ranks(taxonomy, by_rank = "Gattng"),
    "\"Gattng\" is not a valid rank"
  )
  expect_error(
    count_ranks(taxonomy, by_rank = "ohne Rang"), 
    "\"ohne Rang\" cannot be used for by_rank"
  )
  expect_error(
    count_ranks(taxonomy, by_rank = "Klasse"),
    "\"Klasse\" is a valid rank, but it does not appear in the taxonomy graph"
  )
})
