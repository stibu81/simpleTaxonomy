library(dplyr, warn.conflict = FALSE)
library(igraph, warn.conflict = FALSE)
library(shiny)

taxonomy <- read_taxonomy(get_example_taxonomy_file())


test_that("plot_taxonomy() with default settings works", {
  plot <- plot_taxonomy(taxonomy)
  expect_s3_class(plot, c("collapsibleTree", "htmlwidget"))
  expect_equal(
    plot$x$data,
    taxonomy %>% 
      add_tooltip(show_images = FALSE) %>%
      graph_as_nested_list()
  )
  expect_equal(
    plot$x$options,
    get_widget_options(taxonomy, link_length = 150, font_size = 12)
  )
})


test_that("plot_taxonomy() can set font size", {
  plot <- plot_taxonomy(taxonomy, font_size = 15)
  expect_equal(plot$x$options$fontSize, 15)
})


test_that("plot_taxonomy() can set link_length", {
  plot <- plot_taxonomy(taxonomy, link_length = 200)
  expect_equal(plot$x$options$linkLength, 200)
})


test_that("plot_taxonomy() works with focus", {
  # focus is equivalent to setting show and full_expand
  expect_equal(
    plot_taxonomy(taxonomy, focus = "Katzen"),
    plot_taxonomy(taxonomy, show = "Katzen", full_expand = "Katzen")
  )
})


test_that("plot_taxonomy() aborts on invalid inputs", {
  expect_error(plot_taxonomy(list()), "is not a taxonomy_graph")
})


test_that("plot_taxonomy() warns if focus is combined with show or full_expand", {
  ref_plot <- plot_taxonomy(taxonomy, focus = "Hunde")
  expect_message(
    expect_equal(
      plot_taxonomy(taxonomy, focus = "Hunde", show = "Tiger"),
      ref_plot
    ),
    "focus has been used together with show and/or full_expand"
  )
  expect_message(
    expect_equal(
      plot_taxonomy(taxonomy, focus = "Hunde", full_expand = "Katzen"),
      ref_plot
    ),
    "focus has been used together with show and/or full_expand"
  )
})


test_that("set_collapsed() modifies the taxonomy graph", {
  # note: the selection of the nodes to uncollapse is done by get_expanded()
  # which is tested below.
  taxonomy_exp <- set_collapsed(taxonomy, "Katzen", NULL, NULL)
  expect_setequal(
    names(V(taxonomy_exp))[!vertex_attr(taxonomy_exp, "collapsed")],
    c("Raubtiere", "Katzenartige")
  )
})


test_that("get_expanded() can show a node", {
  expanded_show <- get_expanded(
    taxonomy,
    show = "Katzen",
    expand_rank = character(),
    full_expand = character()
  )
  expect_setequal(
    names(V(taxonomy))[expanded_show], 
    c("Raubtiere", "Katzenartige")
  )
})


test_that("get_expanded() can fully expand a node", {
  expanded_full <- get_expanded(
    taxonomy,
    show = character(),
    expand_rank = character(),
    full_expand = "Echte Füchse"
  )
  expect_setequal(
    names(V(taxonomy))[expanded_full], 
    c("Echte Füchse", "Marderhund", "Vulpes", "Polarfuchs",
      "Rotfuchs", "Wüstenfuchs")
  )
})


test_that("get_expanded() can expand all nodes of a given rank", {
  expanded_rank <- get_expanded(
    taxonomy,
    show = character(),
    expand_rank = "Unterfamilie",
    full_expand = character()
  )
  expect_setequal(
    names(V(taxonomy))[expanded_rank], 
    c("Kleinkatzen", "Grosskatzen", "Säbelzahnkatzen")
  )
})


test_that("rm_invalid_taxa() removes taxa that don't exist in the graph", {
  expect_message(
    expect_equal(
      rm_invalid_taxa(c("Katzen", "Flusspferde", "Panthera tigris"), taxonomy),
      c("Katzen", "Tiger")
    ),
    "The following taxa.*be ignored: \"Flusspferde\""
  )
})


test_that("get_widget_options() works", {
  expect_equal(
    get_widget_options(taxonomy, link_length = 200, font_size = 13),
    list(
      input = "selected_taxon",
      hierarchy = 1:6,
      linkLength = 200, 
      fontSize = 13,
      tooltip = TRUE,
      collapsed = "collapsed",
      zoomable = TRUE, 
      margin = list(top = 20, bottom = 20, left = 95.2, right = 196.6)
    )
  )
  expect_equal(
    get_widget_options(
      get_subgraph(taxonomy, "Hunde"), link_length = 150, font_size = 11
    ),
    list(
      input = "selected_taxon", 
      hierarchy = 1:4, 
      linkLength = 150, 
      fontSize = 11,
      tooltip = TRUE, 
      collapsed = "collapsed", 
      zoomable = TRUE, 
      margin = list(top = 20, bottom = 20, left = 58, right = 97.6)
    )
  )
})


test_that("add_tooltip() adds tooltips to the taxonomy graph", {
  # note: the tooltips are created by create_tooltips() which is tested below.
  taxonomy_tt <- add_tooltip(taxonomy, show_images = TRUE, image_size = 200)
  expect_contains(names(vertex_attr(taxonomy_tt)), "tooltip")
  expect_match(
    vertex_attr(taxonomy_tt, "tooltip"),
    "^[[:alpha:] ]+</br><strong>[-[:alpha:] ]+</strong>.*</br>\\([[:alpha:] ]+\\)"
  )
})


test_that("create_tooltip() works with images", {
  tooltips <- create_tooltip(taxonomy, show_images = TRUE, image_size = 300)

  # check that all tooltips match the expected format
  # (remove "Eigentliche Bären" because it has no image)
  expect_match(
    tooltips[names(V(taxonomy)) != "Eigentliche Bären"],
    paste0("^[[:alpha:] ]+</br><strong>[-[:alpha:] ]+</strong>.*</br>",
           "\\([[:alpha:] ]+\\)</br><img src=.*>$")
  )
  
  # check some examples in more detail
  tooltip_carnivora <- tooltips[names(V(taxonomy)) == "Raubtiere"]
  expect_match(
    tooltip_carnivora,
    "^Ordnung</br><strong>Raubtiere</strong> </br>\\(Carnivora\\)</br><img src=.*>$"
  )
  tooltip_lion <- tooltips[names(V(taxonomy)) == "Löwe"]
  expect_match(
    tooltip_lion,
    "^Art</br><strong>Löwe</strong> </br>\\(Panthera leo\\)</br><img src=.*>$"
  )
})


test_that("create_tooltip() works without images", {
  tooltips <- create_tooltip(taxonomy, show_images = FALSE, image_size = 300)

  expect_match(
    tooltips[names(V(taxonomy)) != "Eigentliche Bären"],
    "^[[:alpha:] ]+</br><strong>[-[:alpha:] ]+</strong>.*</br>\\([[:alpha:] ]+\\)"
  )
  
  # check some examples in more detail
  tooltip_carnivora <- tooltips[names(V(taxonomy)) == "Raubtiere"]
  expect_match(
    tooltip_carnivora,
    "^Ordnung</br><strong>Raubtiere</strong> </br>\\(Carnivora\\)$"
  )
  tooltip_lion <- tooltips[names(V(taxonomy)) == "Löwe"]
  expect_match(
    tooltip_lion,
    "^Art</br><strong>Löwe</strong> </br>\\(Panthera leo\\)$"
  )
  tooltip_smilodon <- tooltips[names(V(taxonomy)) == "Smilodon"]
  expect_match(
    tooltip_smilodon,
    "^Gattung</br><strong>Smilodon</strong> <strong>\u2020</strong></br>\\(Smilodon\\)$"
  )
  tooltip_fox <- tooltips[names(V(taxonomy)) == "Rotfuchs"]
  expect_match(
    tooltip_fox,
    paste0("^Art</br><strong>Rotfuchs</strong> ",
           "<i class=\"fas fa-location-dot\"[^>]+></i> ",
           "<i class=\"far fa-eye\"[^>]+></i>",
           "</br>\\(Vulpes vulpes\\)$")
  )
})


test_that("create_tooltip() can set the image size", {
  # pick feliformia because all taxons have images
  feliformia <- get_subgraph(taxonomy, "Katzenartige")
  tooltips <- create_tooltip(feliformia, show_images = TRUE, image_size = 300)
  expect_match(tooltips, "<img src=.*/300px-.*>$")
  
  tooltips <- create_tooltip(feliformia, show_images = TRUE, image_size = 500)
  expect_match(tooltips, "<img src=.*/500px-.*>$")
})


test_that("set_highlight() can highlight specific taxa", {
  taxonomy_highlight <- set_highlight(taxonomy, highlight = c("Katzen", "Tiger"))
  expect_setequal(
    names(V(taxonomy))[vertex_attr(taxonomy_highlight, "colour") == "#FF0000"],
    c("Katzen", "Tiger")
  )
})


test_that("set_highlight() can highlight taxa where image url is missing", {
  # set one image url to NA ("Echte Bären" is already "not_found")
  taxonomy_highlight <- taxonomy %>% 
    set_vertex_attr("image_url", "Robben", NA_character_) %>% 
    set_highlight(character(), highlight_missing_images = TRUE)
  expect_setequal(
    names(V(taxonomy))[vertex_attr(taxonomy_highlight, "colour") == "#FF0000"],
    c("Eigentliche Bären", "Robben")
  )
})


test_that("set_highlight() can set highlight colour", {
  taxonomy_highlight <- taxonomy %>% 
    set_highlight(highlight = "Rotfuchs", colour = "#D428AF")
  expect_setequal(vertex_attr(taxonomy_highlight, "colour", "Rotfuchs"), "#D428AF")
})


test_that("compute_symbols() works when all columns are present", {
  data <- tibble(
    name = letters[1:5],
    extinct = c(TRUE, FALSE, NA, NA, NA),
    local = c(FALSE, FALSE, TRUE, FALSE, TRUE),
    observed = c(NA, FALSE, FALSE, TRUE, TRUE)
  )
  symbols <- compute_symbols(data)
  expect_type(symbols, "character")
  expect_length(symbols, nrow(data))

  # test each entry separately
  expect_equal(symbols[1], "<strong>\u2020</strong>")
  expect_equal(symbols[2], "")
  expect_equal(symbols[3], as.character(icon("location-dot")))
  expect_equal(symbols[4], as.character(icon("eye")))
  expect_equal(symbols[5], paste(icon("location-dot"), icon("eye")))
})


test_that("compute_symbols() works when column local is missing", {
  data <- tibble(
    name = letters[1:4],
    extinct = c(TRUE, FALSE, NA, TRUE),
    observed = c(NA, FALSE, TRUE, TRUE)
  )
  symbols <- compute_symbols(data)
  expect_type(symbols, "character")
  expect_length(symbols, nrow(data))

  # test each entry separately
  expect_equal(symbols[1], "<strong>\u2020</strong>")
  expect_equal(symbols[2], "")
  expect_equal(symbols[3], as.character(icon("eye")))
  expect_equal(symbols[4], paste("<strong>\u2020</strong>", icon("eye")))
})


test_that("compute_symbols() works when column extinct is missing", {
  data <- tibble(
    name = letters[1:4],
    local = c(TRUE, FALSE, NA, TRUE),
    observed = c(NA, FALSE, TRUE, TRUE)
  )
  symbols <- compute_symbols(data)
  expect_type(symbols, "character")
  expect_length(symbols, nrow(data))

  # test each entry separately
  expect_equal(symbols[1], as.character(icon("location-dot")))
  expect_equal(symbols[2], "")
  expect_equal(symbols[3], as.character(icon("eye")))
  expect_equal(symbols[4], paste(icon("location-dot"), icon("eye")))
})



test_that("compute_symbols() works when column observed is missing", {
  data <- tibble(
    name = letters[1:4],
    extinct = c(TRUE, FALSE, NA, TRUE),
    local = c(NA, FALSE, TRUE, TRUE)
  )
  symbols <- compute_symbols(data)
  expect_type(symbols, "character")
  expect_length(symbols, nrow(data))

  # test each entry separately
  expect_equal(symbols[1], "<strong>\u2020</strong>")
  expect_equal(symbols[2], "")
  expect_equal(symbols[3], as.character(icon("location-dot")))
  expect_equal(symbols[4], paste("<strong>\u2020</strong>", icon("location-dot")))
})


test_that("compute_symbols() works when all columns are missing", {
  data <- tibble(
    name = letters[1:4]
  )
  symbols <- compute_symbols(data)
  expect_equal(symbols, rep("", nrow(data)))
})
