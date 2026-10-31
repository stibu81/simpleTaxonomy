library(stringr)
library(dplyr)
library(readr)
library(igraph)
library(glue)
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


test_that("enrich_local_observed() fills missing metadata and updates the file", {
  data <- tibble(
    parent = c(NA_character_, "Tiere", "Tiere", "Tiere"),
    name = c("Tiere", "Tiger", "Wolf", "Wanderfalke"),
    scientific = c(
      "Animalia", "Panthera tigris", "Canis lupus", "Falco peregrinus"
    ),
    rank = c("Reich", "Art", "Art", "Art"),
    local = c(NA, NA, TRUE, NA),
    observed = c(NA, FALSE, NA, NA)
  )
  local_file(list("taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")))

  local_mocked_bindings(
    get_country_id = function(country) {
      if (country == "Switzerland") 756L else NA
    },
    is_taxon_local = function(taxon, country) {
      local_taxa <- c("Canis lupus", "Falco peregrinus")
      country == "Switzerland" && taxon %in% local_taxa
    },
    is_taxon_observed = function(taxon, user) {
      observed_taxa <- c("Panthera tigris", "Canis lupus")
      user == "me" && taxon %in% observed_taxa
    }
  )

  expect_message(
    taxonomy <- enrich_local_observed(
      "taxonomy.csv",
      local_country = "Switzerland",
      user = "me",
      progress = FALSE
    ),
    "trying to get values for 3 taxa"
  )

  expected_local <- c(NA, FALSE, TRUE, TRUE)
  expected_observed <- c(NA, FALSE, TRUE, FALSE)
  expect_equal(vertex_attr(taxonomy, "local"), expected_local)
  expect_equal(vertex_attr(taxonomy, "observed"), expected_observed)

  taxonomy_from_file <- read_taxonomy("taxonomy.csv")
  expect_equal(vertex_attr(taxonomy_from_file, "local"), expected_local)
  expect_equal(vertex_attr(taxonomy_from_file, "observed"), expected_observed)
})


test_that("enrich_local_observed() validates its metadata arguments", {
  expect_error(
    enrich_local_observed("taxonomy.csv"),
    "specify at least one of `local_country` and `user`"
  )

  data <- tibble(
    parent = c(NA_character_, "Tiere"),
    name = c("Tiere", "Tiger"),
    scientific = c("Animalia", "Panthera tigris"),
    rank = c("Reich", "Art")
  )
  local_file(list("taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")))

  expect_error(
    enrich_local_observed("taxonomy.csv", user = "me"),
    "Column.*observed.*missing"
  )
  expect_error(
    enrich_local_observed("taxonomy.csv", local_country = "Switzerland"),
    "Column.*local.*missing"
  )
})


test_that("enrich_local_observed() detects when no metadata are missing", {
  data <- tibble(
    parent = c(NA_character_, "Tiere"),
    name = c("Tiere", "Tiger"),
    scientific = c("Animalia", "Panthera tigris"),
    rank = c("Reich", "Art"),
    local = c(NA, TRUE),
    observed = c(NA, FALSE)
  )
  local_file(list("taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")))

  local_mocked_bindings(get_country_id = function(...) 756L)

  expect_message(
    taxonomy <- enrich_local_observed(
      "taxonomy.csv", local_country = "Switzerland", user = "me"
    ),
    "No missing meta data"
  )
  expect_equal(vertex_attr(taxonomy, "local"), c(NA, TRUE))
  expect_equal(vertex_attr(taxonomy, "observed"), c(NA, FALSE))
})


test_that("enrich_local_observed() fails early for invalid country", {
  # mock get_country_id() to indicate that no country was found
  local_mocked_bindings(get_country_id = function(...) NA)
  expect_error(
    enrich_local_observed(get_example_taxonomy_file(), local_country = "XYZ"),
    "Country 'XYZ' not found"
  )
})


test_that("enrich_local_observed() processes at most 200 taxa", {
  n_taxa <- 250L
  data <- tibble(
    parent = c(NA_character_, rep("Tiere", n_taxa)),
    name = c("Tiere", paste("Taxon", seq_len(n_taxa))),
    scientific = c("Animalia", paste("Species", seq_len(n_taxa))),
    rank = c("Reich", rep("Art", n_taxa)),
    observed = NA
  )
  local_file(list(
    "taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")
  ))

  local_mocked_bindings(
    fill_meta_attrs = function(
      meta_attrs, local_country, user, progress
    ) {
      # mark all taxa as having been observed
      mutate(meta_attrs, observed = TRUE)
    }
  )

  expect_message(
    taxonomy <- enrich_local_observed(
      "taxonomy.csv",
      user = "me",
      progress = FALSE
    ),
    glue("there are {n_taxa} taxa with missing metadata.*for 200 of them")
  )

  # Check that indeed exactly 200 values for observed were set
  expect_equal(
    vertex_attr(taxonomy, "observed"),
    c(NA, rep(TRUE, 200), rep(NA, n_taxa - 200))
  )
})


test_that("fill_meta_attrs() fills and combines all rows", {
  meta_attrs <- tibble(
    scientific = c("Panthera tigris", "Canis lupus"),
    local = c(NA, TRUE)
  )

  local_mocked_bindings(
    is_taxon_local = function(taxon, country) taxon == "Panthera tigris"
  )

  expect_equal(
    fill_meta_attrs(
      meta_attrs,
      local_country = "Switzerland",
      progress = FALSE
    ),
    tibble(
      scientific = c("Panthera tigris", "Canis lupus"),
      local = c(TRUE, TRUE)
    )
  )
})


test_that("fill_meta_attrs() reports connection failures", {
  options(simpleTaxonomy_taxon_cache = NULL, simpleTaxonomy_country_cache = NULL)
  local_mocked_bindings(
    req_inat = function(...) stop("connection failed!"),
  )

  expect_error(
    fill_meta_attrs(
      tibble(scientific = "Panthera tigris", rank = "species", observed = NA),
      user = "me",
      local_country = NULL,
      progress = FALSE
    ),
    "Connection to .* failed.*connection failed!"
  )
})


test_that("fill_meta_data_one() only replaces missing values", {
  local_mocked_bindings(
    is_taxon_local = function(taxon, country) {
      taxon == "Panthera tigris" && country == "Switzerland"
    },
    is_taxon_observed = function(taxon, user) {
      taxon == "Panthera tigris" && user == "me"
    }
  )

  expect_equal(
    fill_meta_data_one(
      tibble(scientific = "Panthera tigris", local = NA, observed = NA),
      local_country = "Switzerland",
      user = "me"
    ),
    tibble(scientific = "Panthera tigris", local = TRUE, observed = TRUE)
  )
  expect_equal(
    fill_meta_data_one(
      tibble(scientific = "Panthera tigris", local = FALSE, observed = FALSE),
      local_country = "Switzerland",
      user = "me"
    ),
    tibble(scientific = "Panthera tigris", local = FALSE, observed = FALSE)
  )
})


test_that("get_taxon_id() filters results, shortens queries, and caches IDs", {
  options(simpleTaxonomy_taxon_cache = NULL, simpleTaxonomy_country_cache = NULL)
  taxon <- "unbeliiiiiiiievablylongtaxonname"
  urls <- character()
  local_mocked_bindings(
    req_inat = function(url) {
      urls <<- c(urls, url)
      list(
        total_results = 3L,
        results = tibble(
          id = c(42L, 43L, 44L),
          # a taxon appears twice, but "complex" will be filtered out
          name = c(taxon, taxon, "different taxon"),
          rank = c("species", "complex", "species")
        )
      )
    }
  )

  expect_equal(get_taxon_id(taxon), 42L)
  expect_equal(get_taxon_id(taxon), 42L)
  # check that only one request was sent to confirm caching
  expect_length(urls, 1L)
  expect_equal(
    getOption("simpleTaxonomy_taxon_cache"),
    list(taxon = taxon, id = 42L)
  )
  expect_match(urls, paste0(str_sub(taxon, end = 30), "&"))
})


test_that("get_taxon_id() handles missing and ambiguous taxa", {
  options(simpleTaxonomy_taxon_cache = NULL, simpleTaxonomy_country_cache = NULL)
  local_mocked_bindings(
    req_inat = function(url) {
      if (str_detect(url, "missing")) {
        return(list(total_results = 0L, results = tibble()))
      }
      list(
        total_results = 2L,
        results = tibble(
          id = c(1L, 2L),
          name = c("ambiguous", "ambiguous"),
          rank = c("genus", "species")
        )
      )
    }
  )

  expect_message(
    expect_true(is.na(get_taxon_id("missing"))),
    "Taxon 'missing' not found"
  )
  expect_message(
    expect_true(is.na(get_taxon_id("ambiguous"))),
    "Taxon 'ambiguous' is not unique"
  )
})


test_that("get_country_id() filters results and caches IDs", {
  options(simpleTaxonomy_taxon_cache = NULL, simpleTaxonomy_country_cache = NULL)
  urls <- character()
  local_mocked_bindings(
    req_inat = function(url) {
      urls <<- c(urls, url)
      list(
        total_results = 2L,
        results = data.frame(
          id = c(756L, 99L),
          display_name = c("Switzerland", "Switzerland other"),
          place_type = c(12L, 7L)
        )
      )
    }
  )

  expect_equal(get_country_id("Switzerland"), 756L)
  expect_equal(get_country_id("Switzerland"), 756L)
  # check that only one request was sent to confirm caching
  expect_length(urls, 1L)
  expect_equal(
    getOption("simpleTaxonomy_country_cache"),
    list(country = "Switzerland", id = 756L)
  )
})


test_that("get_country_id() handles missing and ambiguous countries", {
  options(simpleTaxonomy_taxon_cache = NULL, simpleTaxonomy_country_cache = NULL)
  local_mocked_bindings(
    req_inat = function(url) {
      if (str_detect(url, "missing")) {
        return(list(total_results = 0L, results = tibble()))
      }
      list(
        total_results = 2L,
        results = tibble(
          id = c(1L, 2L),
          display_name = c("Ambiguous one", "Ambiguous two"),
          place_type = c(12L, 12L)
        )
      )
    }
  )

  expect_message(
    expect_true(is.na(get_country_id("missing"))),
    "Country 'missing' not found"
  )
  expect_message(
    expect_true(is.na(get_country_id("ambiguous"))),
    "Several countries found for 'ambiguous'"
  )
})
