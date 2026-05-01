#' Download URL for Wikipedia Images
#'
#' Wikipedia pages usually have a thumbnail associated with them. This function
#' uses an API offered by Wikipedia to extract the URL of that image.
#'
#' @param taxa character with the taxa for which an image URL should be
#'  obtained.
#' @param contact contact data to pass on in the user agent of the API call to 
#'  Wikipedia. Starting in 2026, there is a strict rate limit on API calls per 
#'  minute for calls without contact data. There are two possible ways to pass
#'  contact data:
#'  * a string containing an email address.
#'  * a list that specifies an existing Wikipedia account. You need to specify two
#'    named elements: "lang" giving the language code of the Wikipedia where the 
#'    account is registered and "user" giving the user name.
#'    Example: `list(lang = "de", user = "James Bond")`.
#' 
#'  See the [User Agent Policy](https://foundation.wikimedia.org/wiki/Policy:Wikimedia_Foundation_User-Agent_Policy)
#'  for more information.
#' @param size integer giving the width of the image in pixel.
#' @param lang language of the Wikipedia page to use. Available languages
#'  include "de" (German, the default), "en" (English), "fr" (French), and
#'  "es" (Spanish). See
#'  [List of Wikipedias](https://meta.wikimedia.org/wiki/List_of_Wikipedias#All_Wikipedias_ordered_by_number_of_articles)
#'  for a full list of available languages.
#' @param progress Whether to show a progress bar.
#'
#' @return
#' a character vector with URLs. For taxa where no thumbnail was found, the
#' function returns `NA`.
#'
#' @export

get_wikipedia_image_urls <- function(taxa,
                                      contact = NULL,
                                      size = 120,
                                      lang = "de",
                                      progress = TRUE) {
  user_agent <- get_user_agent_string(contact)
  get_wikipedia_image_urls_(taxa, user_agent, size, lang, progress)
}


# workhorse function to download multiple image urls. It is need to allow the 
# user facing functions to determine the user agent string.
get_wikipedia_image_urls_ <- function(taxa,
                                      user_agent = NULL,
                                      size = 120,
                                      lang = "de",
                                      progress = TRUE,
                                      error_call = rlang::caller_env()) {

  # make sure that size is a positive integer
  int_size <- suppressWarnings(as.integer(size))
  if (is.na(int_size) || size <= 0) {
    cli::cli_abort("{size} is not a positive integer.", call = error_call)
  }

  # catch failure when connecting to wikipedia. A possible reason for failure
  # is an invalid language.
  error_call <- rlang::current_call()
  tryCatch(
    suppressWarnings(
      purrr::map_chr(
        taxa,
        \(taxa) get_wikipedia_image_url(taxa,
                                        user_agent = user_agent,
                                        size = int_size,
                                        lang = lang,
                                        error_call = error_call),
        .progress = progress)
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Connection to {lang}.wikipedia.org failed.",
          "i" = "Message: {e$parent$message}",
          "i" = paste("Check your internet connection and make sure,",
                      "\"{lang}\" is a valid Wikipedia language code."),
          ">" = paste0("Look up valid language codes ",
                       "{.href [here](https://meta.wikimedia.org/wiki/",
                       "List_of_Wikipedias#",
                       "All_Wikipedias_ordered_by_number_of_articles)}.")
        ),
        call = error_call
      )
    }
  )

}

get_wikipedia_image_url <- function(taxon, user_agent, size, lang,
                                    error_call = rlang::caller_env()) {
  
  url <- paste0(
    "http://", lang, ".wikipedia.org/w/api.php?action=query&titles=",
    utils::URLencode(taxon),
    "&prop=pageimages&format=json&pithumbsize=", size, "&redirects="
  )
  resp <- resp <- httr2::request(url) %>%
      httr2::req_user_agent(user_agent) %>% 
      httr2::req_perform()
  parsed <- httr2::resp_body_json(resp)$query$pages[[1]]
  if ("thumbnail" %in% names(parsed)) {
    parsed$thumbnail$source
  } else {
    NA_character_
  }
}


#' Enrich a Taxonomy File with URLs to Wikipedia-Images
#'
#' Add URLs to Images from Wikipedia to a file with a taxonomic hierarchy.
#' By default, existing URLs are kept and only missing URLs are filled in.
#'
#' @param file path to the csv file
#' @param delim the delimiter used in the file
#' @param retry Whether to retry images that have not been found previously.
#' @param progress Whether to show a progress bar.
#' @param quiet Should all output be suppressed?
#' @inheritParams get_wikipedia_image_urls
#'
#' @return
#' a `taxonomy_graph` with added image URLs. The file given by `file` is
#' overwritten as a side effect.
#'
#' @export

enrich_taxonomy_with_images <- function(file,
                                        delim = ",",
                                        contact = NULL,
                                        retry = FALSE,
                                        progress = TRUE,
                                        quiet = FALSE) {

  taxonomy <- read_taxonomy(file, delim)

  # get the missing URLs
  vertices <- igraph::vertex_attr(taxonomy)
  # if retry is requested, set images that have not been found to NA
  image_url <- vertices$image_url
  if (retry) image_url[image_url == "not_found"] <- NA_character_

  url_missing <- is.na(image_url)
  n_missing <- sum(url_missing)
  if (n_missing == 0) {
    cli::cli_alert_info("No missing images urls.")
    return(taxonomy)
  }

  if (!quiet) {
    cli::cli_alert_info("try to get images for {sum(url_missing)} taxa.")
  }

  user_agent <- get_user_agent_string(contact)

  # try in turns: common names (de), scientific names (de), scientific names (en)
  new_urls <- vertices$label[url_missing] %>%
    insert_missing_image_urls(
      image_url[url_missing], user_agent, "de", progress, "common names"
    )
  new_urls <- vertices$scientific[url_missing] %>%
    insert_missing_image_urls(new_urls, user_agent, "de", progress, "scientific names")
  new_urls <- vertices$scientific[url_missing] %>%
    insert_missing_image_urls(new_urls, user_agent, "en", progress, "English Wikipedia")

  # If no URL was found, mark this as "not_found" to distinguish this from
  # taxa that have not yet been tried
  new_urls[is.na(new_urls)] <- "not_found"
  image_url[url_missing] <- new_urls
  igraph::vertex_attr(taxonomy, "image_url") <- image_url

  # write the file
  readr::write_delim(as_tibble(taxonomy), file, delim = delim, na = "")

  # print a summary
  if (!quiet) {
    cat("found: ", sum(new_urls != "not_found"),
        "\nfailed: ", sum(new_urls == "not_found"), "\n")
  }

  taxonomy
}


insert_missing_image_urls <- function(taxa, image_url, user_agent, lang, progress, label) {
  url_missing <- is.na(image_url)
  image_url[url_missing] <- get_wikipedia_image_urls_(
    taxa[url_missing],
    user_agent = user_agent,
    progress = ifelse(progress, label, FALSE),
    lang = lang
  )
  image_url
}


# helper function to generate a user agent string according to the policy at
# https://foundation.wikimedia.org/wiki/Policy:Wikimedia_Foundation_User-Agent_Policy
get_user_agent_string <- function(contact = NULL,
                                  bot_name = "simpleTaxonomyBot") {
  if (is.character(contact) && stringr::str_detect(contact, "@")) {
    contact_str = contact
  } else if (is.list(contact) && all(c("lang", "user") %in% names(contact))) {
    contact_str = glue::glue("wikipedia:{contact$lang}; User:{contact$user}")
  # if no id is given, issue a warning and proceed without contact information
  } else {
    cli::cli_warn(
      c(
        "!" = "No contact information was given for the user agent.",
        "i" = paste("This will likely severely limit the number of requests", 
                    "that are accepted by Wikipedia.")
      )
    )
    contact_str = ""
  }

  # check: bot_name must contain the string "bot"
  if (!stringr::str_detect(bot_name, stringr::regex("bot", ignore_case = TRUE))) {
    cli::cli_abort(
      paste(
      "`bot_name` must contain the string 'bot' in any combination of", 
      "lowercase or uppercase letters."
      )
    )
  }

  version <- utils::packageVersion("simpleTaxonomy")

  glue::glue("User-Agent: {bot_name}/{version} ({contact_str})")
}