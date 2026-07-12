test_that("save_taxonomy writes an html file for a taxonomy widget", {
  widget <- plot_taxonomy(read_taxonomy(get_example_taxonomy_file()), focus = "Katzen")
  out_file <- tempfile(fileext = ".html")

  save_taxonomy(widget, out_file, title = "TestTitle")

  expect_true(file.exists(out_file))

  html <- paste(readLines(out_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "<title>TestTitle</title>", fixed = TRUE)
  expect_match(html, "selected_taxon", fixed = TRUE)
})
