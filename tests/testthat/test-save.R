library(withr)

test_that("save_taxonomy() writes an html file containing a widget", {
  taxonomy <- read_taxonomy(get_example_taxonomy_file())
  widget <- plot_taxonomy(taxonomy)

  local_file("taxonomy_widget.html")
  save_taxonomy(widget, "taxonomy_widget.html", title = "TestTitle", background = "green")
  expect_true(file.exists("taxonomy_widget.html"))

  # test a few expected properties of the widget
  html <- paste(readLines("taxonomy_widget.html", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # check background colour
  expect_match(html, "<body style=\"background-color: green;\">", fixed = TRUE)
  # check that title is set
  expect_match(html, "<title>TestTitle</title>", fixed = TRUE)
  # check that the custom css has been included
  expect_match(html, ".btn-rounded {", fixed = TRUE)
  # check that the node for "Tiger" exists and is set correctly
  expect_match(html, "<strong>Tiger<\\/strong> <\\/br>", fixed = TRUE)
  # check scientific name for Tiger
  expect_match(html, "(Panthera tigris)", fixed = TRUE)
})
