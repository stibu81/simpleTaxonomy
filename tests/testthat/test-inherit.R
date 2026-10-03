library(igraph)
library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(withr)

test_that("inherit_up() works", {
  edges <- tibble(
    parent = c("A", "B", "C", "C", "B", "F", "F"),
    name = c("B", "C", "D", "E", "F", "G", "H")
  )
  vertices <- tibble(
    name = c("A", "B", "C", "D", "E", "F", "G", "H"),
    local = c(NA, NA, NA, TRUE, NA, TRUE, NA, NA)
  )
  graph <- graph_from_data_frame(edges, directed = TRUE, vertices = vertices)
  expect_equal(
    vertex_attr(inherit_up(graph, "local"), "local"),
    c(TRUE, TRUE, TRUE, TRUE, NA, NA, NA, NA)
  )
})


test_that("inherit_down() works", {
  edges <- tibble(
    parent = c("A", "B", "C", "C", "B", "F", "F"),
    name = c("B", "C", "D", "E", "F", "G", "H")
  )
  vertices <- tibble(
    name = c("A", "B", "C", "D", "E", "F", "G", "H"),
    extinct = c(NA, NA, NA, TRUE, NA, TRUE, NA, NA)
  )
  graph <- graph_from_data_frame(edges, directed = TRUE, vertices = vertices)
  expect_equal(
    vertex_attr(inherit_down(graph, "extinct"), "extinct"),
    c(NA, NA, NA, TRUE, NA, TRUE, TRUE, TRUE)
  )
})


test_that("inherit_meta_data() works", {
  data <- tibble(
    parent = c(NA_character_, rep("Katzen", 3), 
               "Kleinkatzen", "Grosskatzen", "Säbelzahnkatzen"),
    name = c("Katzen", "Kleinkatzen", "Grosskatzen", "Säbelzahnkatzen",
             "Eurasischer Luchs", "Löwe", "Smilodon"),
    scientific = c("Felidae", "Felinae", "Pantherinae", "Machairodontinae",
                   "Lynx lynx", "Panthera leo", "Smilodon"),
    rank = c("Familie", rep("Unterfamilie", 3), rep("Art", 3)),
    local = NA,
    observed = NA,
    extinct = NA
  )
  data$local[5] <- TRUE
  data$observed[6] <- TRUE
  data$extinct[4] <- TRUE 
  local_file(list("taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")))

  taxonomy <- inherit_meta_data("taxonomy.csv")

  # test the returned object
  expect_equal(vertex_attr(taxonomy, "local"), c(TRUE, TRUE, NA, NA, TRUE, NA, NA))
  expect_equal(
    vertex_attr(taxonomy, "observed"),
    c(TRUE, NA, TRUE, NA, NA, TRUE, NA)
  )
  expect_equal(vertex_attr(taxonomy, "extinct"), c(NA, NA, NA, TRUE, NA, NA, TRUE))

  # compare the file with the returned object
  expect_equal(as_tibble(read_taxonomy("taxonomy.csv")), as_tibble(taxonomy))
})
