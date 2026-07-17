library(withr)
library(dplyr)

taxonomy <- read_taxonomy(get_example_taxonomy_file())

test_that("run_taxonomy prepares the shiny app without launching it", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyWidgets")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("logger")


  local_options(
    list(
      simpleTaxonomy_file = NULL,
      simpleTaxonomy_root = NULL,
      simpleTaxonomy_expand_ranks = NULL,
      simpleTaxonomy_image_size = NULL,
      simpleTaxonomy_link_length = NULL
    )
  )

  app <- run_taxonomy(
    file = get_example_taxonomy_file(),
    root = "Katzen",
    expand_ranks = c("Gattung", "Art"),
    image_size = "120",
    link_length = 250
  )

  expect_s3_class(app, "shiny.appobj")
  expect_equal(getOption("simpleTaxonomy_root"), "Katzen")
  expect_equal(getOption("simpleTaxonomy_file"), get_example_taxonomy_file())
  expect_equal(getOption("simpleTaxonomy_expand_ranks"), c("Gattung", "Art"))
  expect_equal(getOption("simpleTaxonomy_image_size"), "120")
  expect_equal(getOption("simpleTaxonomy_link_length"), 250)
})


test_that("create_counts_dt() returns NULL when it should", {
  skip_if_not_installed("DT")

  expect_null(
    create_counts_dt(
      taxonomy,
      root = "",
      by_rank = "",
      only_major_ranks = FALSE,
      show_all = FALSE
    )
  )

  expect_null(
    create_counts_dt(
      NULL,
      root = "",
      by_rank = "",
      only_major_ranks = FALSE,
      show_all = FALSE
    )
  )
})


test_that("create_counts_dt() builds a datatable with a rank summary", {
  skip_if_not_installed("DT")

  counts_dt <- create_counts_dt(
    taxonomy,
    root = "Katzen",
    by_rank = "",
    only_major_ranks = FALSE,
    show_all = FALSE
  )

  expect_s3_class(counts_dt, c("datatables", "htmlwidget"))
  expect_equal(
    counts_dt$x$data %>% mutate(Rangstufe = as.character(Rangstufe)),
    data.frame(
      Rangstufe = c("Familie", "Unterfamilie", "Gattung", "Art"),
      Anzahl = c(1, 2, 3, 12)
    )
  )
})


test_that("create_counts_dt() can output only major ranks", {
  skip_if_not_installed("DT")

  counts_dt <- create_counts_dt(
    taxonomy,
    root = "Katzen",
    by_rank = "",
    only_major_ranks = TRUE,
    show_all = FALSE
  )

  expect_s3_class(counts_dt, c("datatables", "htmlwidget"))
  expect_equal(
    counts_dt$x$data %>% mutate(Rangstufe = as.character(Rangstufe)),
    data.frame(
      Rangstufe = c("Familie", "Gattung", "Art"),
      Anzahl = c(1, 3, 12)
    )
  )
})


test_that("create_counts_dt() can group by rank", {
  skip_if_not_installed("DT")

  counts_dt <- create_counts_dt(
    taxonomy,
    root = "Katzen",
    by_rank = "Unterfamilie",
    only_major_ranks = FALSE,
    show_all = FALSE
  )

  expect_s3_class(counts_dt, c("datatables", "htmlwidget"))
  expect_equal(
    counts_dt$x$data,
    data.frame(
      Unterfamilie = c("Kleinkatzen", "Grosskatzen"),
      Gattung = c(2, 1),
      Art = c(6, 6)
    )
  )
})


test_that("create_counts_dt() does not group if by_rank = 'ohne'", {
  skip_if_not_installed("DT")

  counts_dt <- create_counts_dt(
    taxonomy,
    root = "Katzen",
    by_rank = "ohne",
    only_major_ranks = FALSE,
    show_all = FALSE
  )

  expect_s3_class(counts_dt, c("datatables", "htmlwidget"))
  expect_equal(
    counts_dt$x$data %>% mutate(Rangstufe = as.character(Rangstufe)),
    data.frame(
      Rangstufe = c("Familie", "Unterfamilie", "Gattung", "Art"),
      Anzahl = c(1, 2, 3, 12)
    )
  )
})


test_that("create_wiki_button() creates a button with a link to wikipedia", {
  skip_if_not_installed("shiny")

  button <- create_wiki_button(taxonomy, "Hauskatze", id = 3)

  expect_s3_class(button, "shiny.tag")
  # check class, id and on-click action of the button
  expect_match(button$attribs$class, "action-button")
  expect_equal(button$attribs$id, "wiki_button3")
  expect_equal(
    button$attribs$onclick,
    "window.open(\"https://de.wikipedia.org/wiki/Felis_catus\", \"_blank\")"
  )
  # the first child is the icon. Check that it is the wikipedia-icon
  expect_match(
    button$children[[1]]$children[[1]]$attribs$class,
    "fa-wikipedia-w"
  )
  # the second child is the label. Check that it is "Hauskatze".
  expect_equal(unname(button$children[[2]]$children[[1]]), "Hauskatze")
})

test_that("create_wiki_button() returns null if no taxon is passed", {
  skip_if_not_installed("shiny")
  expect_null(create_wiki_button(taxonomy, ""))
})


test_that("flex_row() combines multiple tags into a row", {
  skip_if_not_installed("shiny")
  row <- flex_row(shiny::div("a"), shiny::div("b"), gap = 3)
  expect_s3_class(row, "shiny.tag")
  expect_equal(row$attribs$class, "d-flex align-items-start gap-3")
  expect_equal(as.character(row$children[[1]]), "<div>a</div>")
  expect_equal(as.character(row$children[[2]]), "<div>b</div>")
})

test_that("flex_row() aborts if gap is not numeric", {
  expect_error(flex_row(gap = "wide"), "gap must be a numeric value")
})
