library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(igraph)

taxonomy <- read_taxonomy(get_example_taxonomy_file())

test_that("get_root_node() works", {
  expect_equal(names(get_root_node(taxonomy)), "Raubtiere")
})


test_that("get_leaf_nodes() works", {
  expect_length(get_leaf_nodes(taxonomy), 64)
})


test_that("get_tree_depth() works", {
  expect_equal(get_tree_depth(taxonomy), 6)
})


test_that("get_deepest_nodes() works", {
  catlikes <- get_subgraph(taxonomy, "Katzenartige")
  expect_setequal(
    names(get_deepest_nodes(catlikes)),
    c("Europäische Wildkatze", "Hauskatze", "Ozelot", "Tiger",
      "Jaguar", "Löwe", "Leopard", "Schneeleopard")
  )
})


test_that("get_subgraph() works", {
  cats <- get_subgraph(taxonomy, "Katzen")
  expect_s3_class(cats, "taxonomy_graph")
  expect_equal(names(get_root_node(cats)), "Katzen")
  expect_equal(get_tree_depth(cats), 4)
  expect_equal(names(vertex.attributes(cats)), names(vertex.attributes(taxonomy)))

  housecat <- get_subgraph(taxonomy, "Felis catus")
  expect_s3_class(housecat, "taxonomy_graph")
  expect_equal(names(get_root_node(housecat)), "Hauskatze")
  expect_equal(names(V(housecat)), "Hauskatze")
  expect_equal(names(vertex.attributes(cats)), names(vertex.attributes(taxonomy)))
})


test_that("get_subgraph() aborts on invalid inputs", {
  expect_error(get_subgraph(list()), "is not a taxonomy_graph")
  expect_error(
    get_subgraph(taxonomy, c("Katzen", "Hunde")),
    "taxon must have length 1"
  )
  expect_error(
    get_subgraph(taxonomy, c("Pferde")),
    "\"Pferde\" does not exist in the graph"
  )
})


test_that("create_taxonomy_graph() works", {
  data <- tibble(
    parent = c(NA_character_, "Katzen"),
    name = c("Katzen", "Hauskatze"),
    scientific = c("Felidae", "Felis catus"),
    rank = c("Familie", "Art"),
    image_url = "not_found"
  )
  taxonomy <- create_taxonomy_graph(data)

  expect_s3_class(taxonomy, "taxonomy_graph")
  expect_named(edge.attributes(taxonomy), character(0))
  expect_named(
    vertex.attributes(taxonomy),
    c("name", "scientific", "rank", "image_url")
  )
  expect_contains(names(attributes(taxonomy)), "match_labs")
  expect_mapequal(
    attr(taxonomy, "match_labs"),
    c(Katzen = "Katzen", Hauskatze = "Hauskatze",
      Felidae = "Katzen", "Felis catus" = "Hauskatze")
  )
})


test_that("create_taxonomy_graph() aborts if the graph is not a tree", {
  # multiple roots
  data <- tibble(
    parent = c(NA_character_, NA_character_, "Katzen", "Hunde"),
    name = c("Katzen", "Hunde", "Tiger", "Wolf")
  )
  expect_error(create_taxonomy_graph(data), "graph is not a tree")
  
  # loop
  data <- tibble(
    parent = c("Tiger", "Katzen", "Grosskatzen"),
    name = c("Katzen", "Grosskatzen", "Tiger")
  )
  expect_error(create_taxonomy_graph(data), "graph is not a tree")
})


test_that("as_tibble() recreates the original data table", {
  expect_equal(
    as_tibble(taxonomy),
    read_taxonomy_file(get_example_taxonomy_file(), ",")
  )
})


test_that("as_tibble() drops column image_url if it's empty", {
  taxonomy_no_images <- set_vertex_attr(taxonomy, "image_url", value = NA_character_)
  expect_disjoint(
    names(as_tibble(taxonomy_no_images)),
    "image_url"
  )
})


test_that("graph_as_nested_list() works", {
  data <- tibble(
    parent = c(NA_character_, "Katzen", "Katzen",
               "Kleinkatzen", "Kleinkatzen", "Grosskatzen"),
    name = c("Katzen", "Kleinkatzen", "Grosskatzen",
             "Puma", "Hauskatze", "Tiger"),
    scientific = c("Felidae", "Felinae", "Pantherinae",
                   "Puma concolor", "Felis catus", "Panthera tigris"),
    rank = c("Familie", "Unterfamilie", "Unterfamilie", "Art", "Art", "Art"),
    image_url = "not_found"
  )
  taxonomy <- data %>%
    prepare_taxonomy_df() %>%
    # use placeholder strings for tooltip and colour to simplify the tests
    mutate(tooltip = "tooltip", colour = "colour") %>% 
    create_taxonomy_graph()

  taxonomy_list <- graph_as_nested_list(taxonomy)

  # build the reference structure
  ref <- list(
    name = "Katzen",
    collapsed = TRUE,
    fill = "colour",
    tooltip = "tooltip",
    children = list(
      list(
        name = "Kleinkatzen",
        collapsed = TRUE,
        fill = "colour",
        tooltip = "tooltip",
        children = list(
          list(
            name = "Puma",
            collapsed = TRUE,
            fill = "colour",
            tooltip = "tooltip"
          ),
          list(
            name = "Hauskatze",
            collapsed = TRUE,
            fill = "colour",
            tooltip = "tooltip"
          )
        )
      ),
      list(
        name = "Grosskatzen",
        collapsed = TRUE,
        fill = "colour",
        tooltip = "tooltip",
        children = list(
          list(
            name = "Tiger",
            collapsed = TRUE,
            fill = "colour",
            tooltip = "tooltip"
          )
        )
      )
    )
  )
  expect_equal(taxonomy_list, ref)
})

