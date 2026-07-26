library(stringr)

test_that("flag_icon() returns an img tag with embedded svg data uri", {
  skip_if_not_installed("base64enc")

  tag <- flag_icon("ch")

  expect_s3_class(tag, "shiny.tag")
  expect_equal(tag$name, "img")
  expect_match(tag$attribs$src, "^data:image/svg\\+xml;base64,")
  expect_equal(tag$attribs$class, "flag flag-ch")

  # check that the decoded tag contains the swiss flag
  swiss_svg <- readLines(
      system.file("flags", "svg", "ch.svg", package = "simpleTaxonomy")
    )
  expect_equal(
    # extract the base64 part, decode, split into lines, remove empty lines
    str_remove(tag$attrib$src, "^.*base64,") %>% 
      base64enc::base64decode() %>% 
      rawToChar() %>% 
      str_split_1("\n") %>% 
      Filter(f = \(x) nchar(x) > 0),
    swiss_svg
  )
})


test_that("flag_icon() normalises code case and whitespace", {
  skip_if_not_installed("base64enc")

  tag <- flag_icon("  Ch  ")

  expect_match(tag$attribs$src, "^data:image/svg\\+xml;base64,")
  expect_equal(tag$attribs$class, "flag flag-ch")
})


test_that("flag_icon() errors for invalid type, length, and missing values", {
  expect_error(flag_icon(character()), "code must be a single character value")
  expect_error(flag_icon(NULL), "code must be a single character value")
  expect_error(flag_icon(c("de", "ch")), "code must be a single character value")
  expect_error(flag_icon(NA_character_), "code must be a single character value")
  expect_error(flag_icon(1), "code must be a single character value")
})


test_that("flag_icon() errors for empty code", {
  expect_error(flag_icon(""), "code must not be empty")
  expect_error(flag_icon("   \t"), "code must not be empty")
})


test_that("flag_icon() errors if svg file does not exist", {
  expect_error(flag_icon("xy"), "was not found in package assets")
})
