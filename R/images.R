#' Download URL for Wikipedia Images
#'
#' Wikipedia pages usually have a thumbnail associated with them. This function
#' uses an API offered by Wikipedia to extract the URL of that image.
#'
#' @param taxa character with the taxa for which an image URL should be
#'  obtained.
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
                                     size = 100,
                                     lang = "de",
                                     progress = TRUE) {

  # make sure that size is a positive integer
  int_size <- suppressWarnings(as.integer(size))
  if (is.na(int_size) || size <= 0) {
    cli::cli_abort("{size} is not a positive integer.")
  }

  # catch failure when connecting to wikipedia. A possible reason for failure
  # is an invalid language.
  error_call <- rlang::current_call()
  tryCatch(
    suppressWarnings(
      purrr::map_chr(taxa, get_wikipedia_image_url,
                     size = int_size,
                     lang = lang,
                   .progress = progress)
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Connection to {lang}.wikipedia.org failed.",
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

get_wikipedia_image_url <- function(taxon, size, lang,
                                    error_call = rlang::caller_env()) {

  url <- paste0(
    "http://", lang, ".wikipedia.org/w/api.php?action=query&titles=",
    utils::URLencode(taxon),
    "&prop=pageimages&format=json&pithumbsize=", size, "&redirects="
  )
  parsed <- jsonlite::fromJSON(url)$query$pages[[1]]
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
#'
#' @return
#' a `taxonomy_graph` with added image URLs. The file given by `file` is
#' overwritten as a side effect.
#'
#' @export

enrich_taxonomy_with_images <- function(file,
                                        delim = ",",
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
  if (!quiet) cat("try to get images for", sum(url_missing), "taxa.\n")

  # try in turns: common names (de), scientific names (de), scientific names (en)
  new_urls <- vertices$label[url_missing] %>%
    insert_missing_image_urls(image_url[url_missing], "de", progress, "common names")
  new_urls <- vertices$scientific[url_missing] %>%
    insert_missing_image_urls(new_urls, "de", progress, "scientific names")
  new_urls <- vertices$scientific[url_missing] %>%
    insert_missing_image_urls(new_urls, "en", progress, "English Wikipedia")

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


insert_missing_image_urls <- function(taxa, image_url, lang, progress, label) {
  url_missing <- is.na(image_url)
  image_url[url_missing] <- get_wikipedia_image_urls(
    taxa[url_missing],
    progress = ifelse(progress, label, FALSE),
    lang = lang
  )
  image_url
}
