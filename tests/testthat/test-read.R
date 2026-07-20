library(igraph)
library(dplyr, warn.conflicts = FALSE)

test_that("the example file exists", {
  expect_true(file.exists(get_example_taxonomy_file()))
})


test_that("read_taxonomy() can process the example file", {
  taxonomy <- read_taxonomy(get_example_taxonomy_file())

  expect_s3_class(taxonomy, "taxonomy_graph")
  expect_equal(vcount(taxonomy), 98)
  expect_equal(
    names(vertex.attributes(taxonomy)),
    c("name", "scientific", "rank", "image_url", "label", "colour", "collapsed")
  )
  expect_equal(edge.attributes(taxonomy), setNames(list(), character(0)))
  expect_equal(names(get_root_node(taxonomy)), "Raubtiere")
})


test_that("read_taxonomy() throws error for missing file", {
  expect_error(
    read_taxonomy("file-does-not-exist.csv"),
    "file \"file-does-not-exist.csv\" does not exist."
  )
})


test_that("clean_taxonomy_df() fixes white space issues", {
  # rows test leading, trailing, repeated, mixed
  dirty <- tibble(
    parent = c("  parent node", "parent node  ", "parent   node", " parent  node "),
    name = c("  node name", "node name  ", "node   name", " node  name "),
    scientific = c("  sci name", "sci name  ", "sci   name", " sci  name "),
    rank = c("  node rank", "node rank  ", "node   rank", " node  rank ")
  )

  expect_equal(
    clean_taxonomy_df(dirty),
    tibble(parent = rep("parent node", 4), name = "node name",
           scientific = "sci name", rank = "node rank")
  )
})


test_that("clean_taxonomy_df() removes empty rows", {
  # only rows that are completely empty must be removed
  dirty <- tibble(
    parent = c(NA_character_, NA_character_, "Raubtiere", NA_character_, "Hunde"),
    name = c("Raubtiere", NA_character_, "Hundeartige", NA_character_, NA_character_),
    scientific = c("Carnivora", NA_character_, NA_character_, NA_character_, "Canidae"),
    rank = c("Ordnung", NA_character_, "Unterordnung", NA_character_, "Familie")
  )

  expect_equal(clean_taxonomy_df(dirty), dirty[c(1, 3, 5), ])
})


test_that("check_taxonomy_df() detects missing columns", {
  data <- tibble(
    parent = "parent",
    name = "name",
    rank = "rank"
  )
  expect_error(
    check_taxonomy_df(data),
    "The following required columns are missing: \"scientific\""
  )
})


test_that("check_taxonomy_df() adds column image_url", {
  data <- tibble(
    parent = NA_character_,
    name = "root",
    scientific = "root",
    rank = "rank"
  )
  data <- check_taxonomy_df(data)
  expect_contains(names(data), "image_url")
  expect_equal(data$image_url, NA_character_)
})


test_that("check_taxonomy_df() aborts if root is missing", {
  data <- tibble(
    parent = "parent",
    name = "name",
    scientific = "scientific",
    rank = "rank"
  )
  expect_error(check_taxonomy_df(data), "There is no root taxon")
})


test_that("check_taxonomy_df() aborts if there are multiple roots", {
  data <- tibble(
    parent = NA_character_,
    name = c("root1", "root2"),
    scientific = "scientific",
    rank = "rank"
  )
  expect_error(
    check_taxonomy_df(data), 
    "There are multiple root taxa: \"root1\", \"root2\""
  )
})


test_that("check_taxonomy_df() aborts if there are missing names", {
  data <- tibble(
    parent = c(NA_character_, "Raubtiere"),
    name = c("Raubtiere", NA_character_),
    scientific = c("Carnivora", "Caniformia"),
    rank = c("Ordnung", "Unterordnung")
  )
  expect_error(
    check_taxonomy_df(data),
    # note the row number corresponds to the csv file, which has headers in row 1
    "There are missing names in row\\(s\\): 3"
  )
})


test_that("check_taxonomy_df() aborts if there are duplicate names", {
  data <- tibble(
    parent = c(NA_character_, "Raubtiere"),
    name = c("Raubtiere", "Raubtiere"),
    scientific = c("Carnivora", "Caniformia"),
    rank = c("Ordnung", "Unterordnung")
  )
  expect_error(
    check_taxonomy_df(data), 
    "There are duplicate names: \"Raubtiere\""
  )
})


test_that("check_taxonomy_df() aborts if there are duplicate scientific names", {
  data <- tibble(
    parent = c(NA_character_, "Raubtiere"),
    name = c("Raubtiere", "Hundeartige"),
    scientific = c("Carnivora", "Carnivora"),
    rank = c("Ordnung", "Unterordnung")
  )
  expect_error(
    check_taxonomy_df(data), 
    "There are duplicate scientific names: \"Carnivora\""
  )
})


test_that("check_taxonomy_df() aborts if there are undefined parent taxa", {
  data <- tibble(
    parent = c(NA_character_, "Hundeartige"),
    name = c("Raubtiere", "Hunde"),
    scientific = c("Carnivora", "Canidae"),
    rank = c("Ordnung", "Fammilie")
  )
  expect_error(
    check_taxonomy_df(data),
    "There are undefined parent taxa: \"Hundeartige\""
  )
})


test_that("check_taxonomy_df() aborts if there are missing ranks", {
  data <- tibble(
    parent = c(NA_character_, "Raubtiere"),
    name = c("Raubtiere", "Hundeartige"),
    scientific = c("Carnivora", "Caniformia"),
    rank = c("Ordnung", NA_character_)
  )
  expect_error(
    check_taxonomy_df(data),
    "Some taxa have no rank: \"Hundeartige\""
  )
})


test_that("prepare_taxonomy_df() returns the expected additional columns", {
  data <- tibble(
    parent = c(NA_character_, "Pferde (F)"),
    name = c("Pferde (F)", "Pferde"),
    scientific = c("Equidae", "Equus"),
    rank = c("Familie", "Gattung"),
  )
  
  prepared <- prepare_taxonomy_df(data)
  expect_named(
    prepared,
    c("parent", "name", "scientific", "rank", "label", "colour", "collapsed")
  )
  expect_equal(prepared$label, c("Pferde", "Pferde"))
  expect_match(prepared$colour, "^#[0-9A-F]{6}$")
  expect_all_true(prepared$collapsed)
})


test_that("prepare_taxonomy_df() warns when there are invalid ranks", {
  data <- tibble(
    parent = NA_character_,
    name = "Pferde",
    scientific = "Equus",
    rank = "Gattng",
  )
  expect_message(
    prepared <- prepare_taxonomy_df(data),
    "There are invalid ranks: \"Gattng\""
  )
  expect_equal(prepared$colour, NA_character_)
})


test_that("available_ranks() returns a tibble with the correct format", {
  ar <- available_ranks()
  expect_s3_class(ar, "tbl_df")
  expect_named(ar, c("level", "de"))
  expect_equal(ar$level, c(1:(nrow(ar) - 1), NA_integer_))
  expect_equal(tail(ar$de, 1), "ohne Rang")
})


test_that("is_url() works", {
  expect_true(is_url("https://en.wikipedia.org"))
  expect_true(is_url("http://en.wikipedia.org"))
  expect_false(is_url("inst/example/carnivora.csv"))
})


test_that("get_taxon_labels() works", {
  expect_equal(
    get_taxon_labels(c("Pferde (F)", "Katze", "Eisvögel (Schmetterlinge)")),
    c("Pferde", "Katze", "Eisvögel")
  )
})
