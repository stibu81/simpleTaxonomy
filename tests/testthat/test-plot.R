test_that("plot helpers resolve valid taxa and expansion paths", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  expect_message(
    valid_taxa <- simpleTaxonomy:::rm_invalid_taxa(c("Felis catus", "nope"), graph),
    "do not exist"
  )
  expect_equal(unname(valid_taxa), "Hauskatze")

  expanded_show <- names(igraph::V(graph))[
    simpleTaxonomy:::get_expanded(
      graph,
      show = "Katzen",
      expand_rank = character(),
      full_expand = character()
    )
  ]
  expect_equal(expanded_show, c("Raubtiere", "Katzenartige"))

  expanded_full <- names(igraph::V(graph))[
    simpleTaxonomy:::get_expanded(
      graph,
      show = character(),
      expand_rank = character(),
      full_expand = "Katzen"
    )
  ]
  expect_true(all(c("Katzen", "Hauskatze", "Tiger") %in% expanded_full))
})


test_that("tooltip and highlight helpers update graph attributes", {
  graph <- read_taxonomy(get_example_taxonomy_file())

  tooltip <- simpleTaxonomy:::create_tooltip(graph, show_images = TRUE, image_size = "250")
  expect_match(tooltip[[1]], "<strong>Raubtiere</strong>", fixed = TRUE)
  expect_match(tooltip[[1]], "/250px-", fixed = TRUE)

  highlighted <- simpleTaxonomy:::set_highlight(graph, highlight = "Katzen")
  expect_equal(
    igraph::vertex_attr(highlighted, "colour")[names(igraph::V(highlighted)) == "Katzen"],
    "#FF0000"
  )

  missing_images <- simpleTaxonomy:::set_highlight(
    graph,
    highlight = character(),
    highlight_missing_images = TRUE
  )
  expect_equal(sum(igraph::vertex_attr(missing_images, "colour") == "#FF0000"), 1)
})


test_that("plot_taxonomy returns a configured widget", {
  widget <- plot_taxonomy(
    read_taxonomy(get_example_taxonomy_file()),
    focus = "Katzen",
    show_images = TRUE,
    link_length = 175,
    font_size = 14
  )

  expect_s3_class(widget, "htmlwidget")
  expect_equal(widget$x$options$input, "selected_taxon")
  expect_equal(widget$x$options$linkLength, 175)
  expect_equal(widget$x$options$fontSize, 14)
  expect_equal(widget$x$data$name, "Raubtiere")

  expect_error(plot_taxonomy(list()), "not a taxonomy_graph")
})
