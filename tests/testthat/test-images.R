test_that("get_wikipedia_image_urls delegates through the generated user agent", {
  testthat::local_mocked_bindings(
    get_user_agent_string = function(contact = NULL,
                                     bot_name = "simpleTaxonomyBot") {
      "mock-agent"
    },
    get_wikipedia_image_urls_ = function(taxa,
                                         user_agent = NULL,
                                         size = 120,
                                         lang = "de",
                                         progress = TRUE,
                                         error_call = rlang::caller_env()) {
      expect_equal(user_agent, "mock-agent")
      expect_equal(size, 120)
      expect_equal(lang, "de")
      expect_false(progress)
      rep("https://example.org/image.jpg", length(taxa))
    },
    .package = "simpleTaxonomy"
  )

  expect_equal(
    get_wikipedia_image_urls(c("A", "B"), contact = "me@example.com", progress = FALSE),
    rep("https://example.org/image.jpg", 2)
  )
})


test_that("image url helpers validate size and only fill missing entries", {
  expect_error(
    simpleTaxonomy:::get_wikipedia_image_urls_(c("A"), size = 0, progress = FALSE),
    "not a positive integer"
  )

  testthat::local_mocked_bindings(
    get_wikipedia_image_urls_ = function(taxa,
                                         user_agent = NULL,
                                         size = 120,
                                         lang = "de",
                                         progress = TRUE,
                                         error_call = rlang::caller_env()) {
      paste0("https://example.org/", seq_along(taxa), ".jpg")
    },
    .package = "simpleTaxonomy"
  )

  expect_equal(
    simpleTaxonomy:::insert_missing_image_urls(
      taxa = c("A", "B"),
      image_url = c(NA_character_, "keep"),
      user_agent = "ua",
      lang = "de",
      progress = FALSE,
      label = "test"
    ),
    c("https://example.org/1.jpg", "keep")
  )
})


test_that("get_user_agent_string handles different contact formats", {
  expect_warning(
    ua_missing <- simpleTaxonomy:::get_user_agent_string(),
    "No contact information"
  )
  expect_match(ua_missing, "simpleTaxonomyBot/", fixed = TRUE)

  ua_email <- simpleTaxonomy:::get_user_agent_string("me@example.com")
  expect_match(ua_email, "me@example.com", fixed = TRUE)

  ua_user <- simpleTaxonomy:::get_user_agent_string(
    list(lang = "de", user = "Jane Bot")
  )
  expect_match(ua_user, "wikipedia:de; User:Jane Bot", fixed = TRUE)

  expect_error(
    simpleTaxonomy:::get_user_agent_string("me@example.com", bot_name = "simpleTaxonomy"),
    "must contain the string 'bot'"
  )
})


test_that("enrich_taxonomy_with_images can update retried missing URLs", {
  tmp_file <- tempfile(fileext = ".csv")
  expect_true(file.copy(get_example_taxonomy_file(), tmp_file, overwrite = TRUE))

  testthat::local_mocked_bindings(
    get_wikipedia_image_urls_ = function(taxa,
                                         user_agent = NULL,
                                         size = 120,
                                         lang = "de",
                                         progress = TRUE,
                                         error_call = rlang::caller_env()) {
      rep("https://example.org/fill.jpg", length(taxa))
    },
    .package = "simpleTaxonomy"
  )

  out <- enrich_taxonomy_with_images(
    tmp_file,
    retry = TRUE,
    progress = FALSE,
    quiet = TRUE,
    contact = "me@example.com"
  )

  reloaded <- readr::read_delim(tmp_file, delim = ",", col_types = "c")
  expect_s3_class(out, "taxonomy_graph")
  expect_equal(
    reloaded$image_url[reloaded$name == "Eigentliche Bären"],
    "https://example.org/fill.jpg"
  )
})
