test_that("run_taxonomy prepares the shiny app without launching it", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("shinyWidgets")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("logger")

  old_options <- options(
    simpleTaxonomy_file = NULL,
    simpleTaxonomy_root = NULL,
    simpleTaxonomy_expand_ranks = NULL,
    simpleTaxonomy_image_size = NULL,
    simpleTaxonomy_link_length = NULL
  )
  on.exit(options(old_options), add = TRUE)

  app <- run_taxonomy(
    file = get_example_taxonomy_file(),
    root = "Katzen",
    launch_browser = FALSE
  )

  expect_s3_class(app, "shiny.appobj")
  expect_equal(getOption("simpleTaxonomy_root"), "Katzen")
  expect_match(getOption("simpleTaxonomy_file"), "carnivora\\.csv$")
})


test_that("create_counts_dt builds a datatable for package summaries", {
  testthat::skip_if_not_installed("DT")

  graph <- read_taxonomy(get_example_taxonomy_file())

  expect_null(
    create_counts_dt(
      graph,
      root = "",
      by_rank = "",
      only_major_ranks = FALSE,
      show_all = FALSE
    )
  )

  counts_dt <- create_counts_dt(
    graph,
    root = "Katzen",
    by_rank = "",
    only_major_ranks = FALSE,
    show_all = FALSE
  )

  expect_s3_class(counts_dt, "htmlwidget")
  expect_equal(names(counts_dt$x$data), c("Rangstufe", "Anzahl"))
  expect_equal(nrow(counts_dt$x$data), 4)
})


test_that("shiny helper UI functions build the expected tags", {
  testthat::skip_if_not_installed("shiny")

  button <- create_wiki_button(read_taxonomy(get_example_taxonomy_file()), "Hauskatze", id = 7)
  expect_s3_class(button, "shiny.tag")
  expect_equal(button$attribs$id, "wiki_button7")
  expect_match(button$attribs$onclick, "Felis_catus", fixed = TRUE)
  expect_null(create_wiki_button(read_taxonomy(get_example_taxonomy_file()), ""))

  row <- flex_row(shiny::div("a"), shiny::div("b"), gap = 3)
  expect_s3_class(row, "shiny.tag")
  expect_equal(row$attribs$class, "d-flex align-items-start gap-3")

  expect_error(flex_row(gap = "wide"), "gap must be a numeric value")
})
