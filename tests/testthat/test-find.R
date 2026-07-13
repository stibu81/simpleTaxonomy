taxonomy <- read_taxonomy(get_example_taxonomy_file())

test_that("find_taxon() searches common and scientific names", {
  hits <- find_taxon(taxonomy, "katze")
  expect_all_true(c("Katzen", "Hauskatze") %in% hits)

  expect_equal(
    find_taxon(taxonomy, "felis"),
    c("Echte Katzen", "Europäische Wildkatze", "Hauskatze", "Nebelparder")
  )
})


test_that("find_taxon() can be restricted to common or scientific names", {
  expect_equal(find_taxon(taxonomy, "fossa"), c("Fossa", "Fanaloka"))
  expect_equal(find_taxon(taxonomy, "fossa", target = "name"), "Fossa")
  expect_equal(find_taxon(taxonomy, "fossa", target = "scientific"), "Fanaloka")
})


test_that("get_taxon_names() performs exact label lookup", {
  expect_equal(
    get_taxon_names(taxonomy, c("Carnivora", "Hauskatze", "Felis catus")),
    setNames(
      c("Raubtiere", "Hauskatze", "Hauskatze"),
      c("Carnivora", "Hauskatze", "Felis catus")
    )
  )
  expect_equal(get_taxon_names(taxonomy, "Sphinx"), setNames(NA_character_, "Sphinx"))
})


test_that("get_parent_taxon() returns parent labels", {
  expect_equal(get_parent_taxon(taxonomy, "Katzen"), "Katzenartige")
  expect_equal(get_parent_taxon(taxonomy, "Felidae"), "Katzenartige")
  expect_identical(get_parent_taxon(taxonomy, "Raubtiere"), character())
})


test_that("get_parent_taxon() abort when inputs are invalid", {
  expect_error(get_parent_taxon(list(), "Katzen"), "not a taxonomy_graph object")
  expect_error(get_parent_taxon(taxonomy, c("Katzen", "Hunde")), "must have length 1")
  expect_error(get_parent_taxon(taxonomy, "Sphinx"), "\"Sphinx\" does not exist")
})
