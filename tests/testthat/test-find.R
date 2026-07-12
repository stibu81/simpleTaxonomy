test_that("find_taxon searches common and scientific names", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  hits <- find_taxon(graph, "katze")
  expect_true(all(c("Katzen", "Hauskatze") %in% hits))

  expect_equal(find_taxon(graph, "Felis catus", target = "scientific"), "Hauskatze")
  expect_false("Hauskatze" %in% find_taxon(graph, "Felis", target = "name"))
})


test_that("get_taxon_names performs exact label lookup", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  expect_equal(
    unname(get_taxon_names(graph, c("Carnivora", "Hauskatze", "Felis catus"))),
    c("Raubtiere", "Hauskatze", "Hauskatze")
  )
  expect_true(is.na(get_taxon_names(graph, "does-not-exist")))
})


test_that("get_parent_taxon returns parent labels and validates input", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  expect_equal(get_parent_taxon(graph, "Katzen"), "Katzenartige")
  expect_identical(get_parent_taxon(graph, "Raubtiere"), character())

  expect_error(get_parent_taxon(list(), "Katzen"), "not a taxonomy_graph")
  expect_error(get_parent_taxon(graph, c("Katzen", "Hunde")), "length 1")
  expect_error(get_parent_taxon(graph, "does-not-exist"), "does not exist")
})
