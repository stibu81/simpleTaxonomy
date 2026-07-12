test_that("read_taxonomy reads the example file into a taxonomy graph", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  expect_s3_class(graph, "taxonomy_graph")
  expect_equal(igraph::vcount(graph), 96)
  expect_equal(names(get_root_node(graph)), "Raubtiere")
})


test_that("read helpers clean data and reject invalid inputs", {
  dirty <- dplyr::tibble(
    parent = c(NA_character_, " Root ", NA_character_),
    name = c(" Root ", " Child  Name ", NA_character_),
    scientific = c("Rootus", "Childus  example", NA_character_),
    rank = c("Ordnung", " Art ", NA_character_)
  )

  cleaned <- simpleTaxonomy:::clean_taxonomy_df(dirty)
  expect_equal(nrow(cleaned), 2)
  expect_equal(cleaned$name[[2]], "Child Name")

  expect_error(
    simpleTaxonomy:::read_taxonomy_file("does-not-exist.csv", ","),
    "does not exist"
  )

  missing_cols <- dplyr::tibble(
    parent = NA_character_,
    name = "Root",
    rank = "Ordnung"
  )
  expect_error(
    simpleTaxonomy:::check_taxonomy_df(missing_cols),
    "required columns are missing"
  )

  dup_sci <- dplyr::tibble(
    parent = c(NA_character_, "Root"),
    name = c("Root", "Child"),
    scientific = c("Same", "Same"),
    rank = c("Ordnung", "Art")
  )
  expect_error(
    simpleTaxonomy:::check_taxonomy_df(dup_sci),
    "duplicate scientific names"
  )
})


test_that("read helpers prepare metadata and expose utility lookups", {
  prepared <- suppressMessages(
    simpleTaxonomy:::prepare_taxonomy_df(
      dplyr::tibble(
        parent = c(NA_character_, "Pferde (F)"),
        name = c("Pferde (F)", "Katze"),
        scientific = c("Equidae", "Felis catus"),
        rank = c("Ordnung", "invalid"),
        image_url = c(NA_character_, NA_character_)
      )
    )
  )

  expect_true(all(c("label", "colour", "collapsed") %in% names(prepared)))
  expect_equal(prepared$label[[1]], "Pferde")
  expect_true(all(prepared$collapsed))
  expect_true(is.na(prepared$colour[[2]]))

  ranks <- available_ranks()
  expect_true(all(c("level", "de") %in% names(ranks)))
  expect_equal(ranks$de[[1]], "Lebewesen")

  expect_true(file.exists(get_example_taxonomy_file()))
  expect_true(simpleTaxonomy:::is_url("https://example.org"))
  expect_false(simpleTaxonomy:::is_url("inst/example/carnivora.csv"))
  expect_equal(
    simpleTaxonomy:::get_taxon_labels(c("Pferde (F)", " Katze ")),
    c("Pferde", "Katze")
  )
})
