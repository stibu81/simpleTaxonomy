library(stringr)
library(dplyr)
library(withr)

test_that("is_taxon_local() works", {
  skip_on_cran()
  expect_true(is_taxon_local("Podismopsis keisti", "Switzerland"))
  expect_false(is_taxon_local("Panthera tigris", "Switzerland"))
})


test_that("is_taxon_local() handles bad input", {
  skip_on_cran()
  expect_message(
    expect_equal(is_taxon_local("bad species", "Switzerland"), NA),
    "Taxon 'bad species' not found"
  )
  expect_message(
    expect_equal(is_taxon_local("Canis lupus", "bad country"), NA),
    "Country 'bad country' not found"

  )
  expect_message(
    expect_equal(is_taxon_local("Canis lupus", "Ch"), NA),
    "Several countries found for 'Ch'"
  )
})


test_that("is_taxon_observed() works", {
  skip_on_cran()
  expect_true(is_taxon_observed("Apis mellifera", user = "Stibu"))
  expect_false(is_taxon_observed("Raphus cucullatus", user = "Stibu"))
})


test_that("is_taxon_observed() handles bad input", {
  skip_on_cran()
  expect_message(
    expect_equal(is_taxon_observed("bad species", user = "stibu"), NA),
    "Taxon 'bad species' not found"
  )
})
