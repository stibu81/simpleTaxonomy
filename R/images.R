#' Download URL for Wikipedia Images
#'
#' Wikipedia pages usually have a thumbnail associated with them. This function
#' uses an API offered by Wikipedia to extract the URL of that image.
#'
#' @param taxa character with the taxa for which an image URL should be
#'  obtained.
#' @param size integer giving the width of the image in pixel.
#' @param progress Whether to show a progress bar.
#'
#' @return
#' a character vector with URLs. For taxa where no thumbnail was found, the
#' function returns `NA`.
#'
#' @export

get_wikipedia_image_urls <- function(taxa, size = 100, progress = TRUE) {

  # make sure that size is a positive integer
  int_size <- suppressWarnings(as.integer(size))
  if (is.na(int_size) || size <= 0) {
    cli::cli_abort("{size} is not a positive integer.")
  }

  purrr::map_chr(taxa, get_wikipedia_image_url,
                 size = int_size,
                 .progress = progress)

}

get_wikipedia_image_url <- function(taxon, size) {

  url <- paste0("http://de.wikipedia.org/w/api.php?action=query&titles=",
                utils::URLencode(taxon),
                "&prop=pageimages&format=json&pithumbsize=", size, "&redirects=")
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
#'
#' @return
#' a `taxonomy_graph` with added image URLs. The file given by `file` is
#' overwritten as a side effect.
#'
#' @export

enrich_taxonomy_with_images <- function(file,
                                        delim = ",",
                                        retry = FALSE,
                                        progress = TRUE) {

  taxonomy <- read_taxonomy(file, delim)

  # get the missing URLs
  vertices <- igraph::vertex_attr(taxonomy)
  # if retry is requested, set images that have not been found to NA
  image_url <- vertices$image_url
  if (retry) image_url[image_url == "not_found"] <- NA_character_
  url_missing <- is.na(image_url)
  new_urls <- get_wikipedia_image_urls(
    vertices$label[url_missing],
    progress = ifelse(progress, "common names", FALSE)
  )

  # for those that failed, try again with the scientific name
  url_missing2 <- is.na(new_urls)
  new_urls2 <- get_wikipedia_image_urls(
    vertices$scientific[url_missing][url_missing2],
    progress = ifelse(progress, "scientific names", FALSE)
  )
  new_urls[url_missing2] <- new_urls2

  # If no URL was found, mark this as "not_found" to distinguish this from
  # taxa that have not yet been tried
  new_urls[is.na(new_urls)] <- "not_found"
  image_url[url_missing] <- new_urls
  igraph::vertex_attr(taxonomy, "image_url") <- image_url

  # write the file
  readr::write_delim(as_tibble(taxonomy), file, delim = delim)

  taxonomy
}


