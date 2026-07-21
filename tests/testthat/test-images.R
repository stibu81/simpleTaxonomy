library(glue)
library(readr)
library(dplyr)
library(igraph)
library(withr)

st_ver <- packageVersion("simpleTaxonomy")
# url of the image in the example data
ref_url <- paste0("https://upload.wikimedia.org/wikipedia/commons/thumb/b/b0/",
                  "Bengal_tiger_%28Panthera_tigris_tigris%29_female_3_crop.jpg/",
                  "250px-Bengal_tiger_%28Panthera_tigris_tigris%29_female_3_crop.jpg")

test_that("get_wikipedia_urls() returns multiple image urls", {
  # mock the actual performing of the request. This will always return the
  # same image url (for tiger).
  local_mocked_bindings(
    req_perform = function(...) dget(test_path("data", "response_tiger.R")),
    .package = "httr2"
  )

  expect_equal(
    get_wikipedia_image_urls(c("tiger", "tiger"), contact = "example@email.com"),
    rep(ref_url, 2)  
  )
})


test_that("get_wikipedia_urls() aborts for invalid size", {
  expect_error(
    get_wikipedia_image_urls("tiger", contact = "example@email.com", size = 0),
    "0 is not a positive integer"
  )
})


test_that("get_wikipedia_urls() handles failing internet connection", {
  local_mocked_bindings(
    req_perform = function(...) stop("connection failed!"),
    .package = "httr2"
  )
  expect_error(
    get_wikipedia_image_urls(c("tiger", "tiger"), contact = "example@email.com"),
    "Connection to de\\.wikipedia\\.org failed.*Message: connection failed!"  
  )
})


test_that("enrich_taxonomy_with_images() works on a file without urls", {
  local_mocked_bindings(
    req_perform = function(...) dget(test_path("data", "response_tiger.R")),
    .package = "httr2"
  )
  data <- tibble(
    parent = c(NA_character_, "Katzen"),
    name = c("Katzen", "Tiger"),
    scientific = c("Felidae", "Panthera tigris"),
    rank = c("Familie", "Art")
  )
  local_file(list("taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")))

  expect_message(
      taxonomy <- enrich_taxonomy_with_images(
        "taxonomy.csv",
        contact = "example@email.com",
        progress = FALSE
      ),
      "try to get images for 2 taxa" 
    ) %>% 
    expect_message("found : 2.*failed: 0")
  # check returned object and the file
  expect_equal(vertex_attr(taxonomy, "image_url"), rep(ref_url, 2))
  expect_equal(
    vertex_attr(read_taxonomy("taxonomy.csv"), "image_url"),
    rep(ref_url, 2)
  )
})


test_that("enrich_taxonomy_with_images() detects no urls need to be fetched", {
  data <- tibble(
    parent = c(NA_character_, "Katzen"),
    name = c("Katzen", "Tiger"),
    scientific = c("Felidae", "Panthera tigris"),
    rank = c("Familie", "Art"),
    image_url = c("https://someimage.jpg", "not_found")
  )
  local_file(list("taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")))

  expect_message(
      taxonomy <- enrich_taxonomy_with_images("taxonomy.csv"),
      "No missing images urls" 
    )
})


test_that("enrich_taxonomy_with_images() can retry", {
  local_mocked_bindings(
    req_perform = function(...) dget(test_path("data", "response_tiger.R")),
    .package = "httr2"
  )
  data <- tibble(
    parent = c(NA_character_, "Katzen"),
    name = c("Katzen", "Tiger"),
    scientific = c("Felidae", "Panthera tigris"),
    rank = c("Familie", "Art"),
    image_url = c("https://someimage.jpg", "not_found")
  )
  local_file(list("taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")))

  expect_message(
      taxonomy <- enrich_taxonomy_with_images(
        "taxonomy.csv",
        contact = "example@email.com",
        retry = TRUE,
        progress = FALSE
      ),
      "try to get images for 1 taxa" 
    ) %>% 
    expect_message("found : 1.*failed: 0")
  # check returned object and the file
  expect_equal(
    vertex_attr(taxonomy, "image_url"), 
    c("https://someimage.jpg", ref_url)
  )
  expect_equal(
    vertex_attr(read_taxonomy("taxonomy.csv"), "image_url"),
    c("https://someimage.jpg", ref_url)
  )
})


test_that("enrich_taxonomy_with_images() writes missing values as empty strings", {
  data <- tibble(
    parent = c(NA_character_, "Katzen"),
    name = c("Katzen", "Tiger"),
    scientific = c("Felidae", "Panthera tigris"),
    rank = c("Familie", "Art"),
    image_url = c("https://someimage.jpg", "not_found"),
    extinct = "",
    local = "",
    observed = ""
  )
  local_file(list("taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")))

  suppressMessages(
    enrich_taxonomy_with_images("taxonomy.csv")
  )
  # read the taxonomy from file without any conversion to NA
  taxonomy_from_file <- read_csv(
    "taxonomy.csv",
    col_types = cols(.default = "c"),
    na = character()
  )
  expect_equal(taxonomy_from_file$parent, c("", "Katzen"))
  expect_equal(taxonomy_from_file$extinct, c("", ""))
  expect_equal(taxonomy_from_file$local, c("", ""))
  expect_equal(taxonomy_from_file$observed, c("", ""))
})


test_that(
  "enrich_taxonomy_with_images() does not create optional columns if they don't exist",
  {
  data <- tibble(
    parent = c(NA_character_, "Katzen"),
    name = c("Katzen", "Tiger"),
    scientific = c("Felidae", "Panthera tigris"),
    rank = c("Familie", "Art"),
    image_url = c("https://someimage.jpg", "not_found")
  )
  local_file(list("taxonomy.csv" = write_csv(data, "taxonomy.csv", na = "")))

  suppressMessages(enrich_taxonomy_with_images("taxonomy.csv"))
  taxonomy_from_file <- read_csv("taxonomy.csv", col_types = cols(.default = "c"))
  expect_disjoint(names(taxonomy_from_file), c("extinct", "local", "observed"))
})


test_that("get_user_agent_string() works with an email address", {
  expect_equal(
    get_user_agent_string("example@email.com"),
    glue("User-Agent: simpleTaxonomyBot/{st_ver} (example@email.com)")
  )
})


test_that("get_user_agent_string() works with user name", {
  expect_equal(
    get_user_agent_string(list(lang = "de", user = "someone")),
    glue("User-Agent: simpleTaxonomyBot/{st_ver} (wikipedia:de; User:someone)")
  )
})


test_that("get_user_agent_string() warns if no contact data are provided", {
  expect_warning(ua_missing <- get_user_agent_string(), "No contact information")
  expect_equal(ua_missing, glue("User-Agent: simpleTaxonomyBot/{st_ver} ()"))
})


test_that("get_user_agent_string() can use a different bot name", {
  expect_equal(
    get_user_agent_string("example@email.com", bot_name = "somebot"),
    glue("User-Agent: somebot/{st_ver} (example@email.com)")
  )
})


test_that("get_user_agent_string() aborts if bot name is not valid", {
  expect_error(
    get_user_agent_string("example@email.com", bot_name = "badname"),
    "`bot_name` must contain the string 'bot'"
  )
})


test_that("get_user_agent_string() warns if invalid contact information is used", {
  # string that is no email
  expect_warning(get_user_agent_string("noemail"), "No contact information")
  # list with missing fields
  expect_warning(get_user_agent_string(list(user = "me")), "No contact information")
  expect_warning(get_user_agent_string(list(lang = "de")), "No contact information")
})
