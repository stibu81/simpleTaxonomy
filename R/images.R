#' Download URL for Wikipedia Images
#'
#' Wikipedia pages usually have a thumbnail associated with them. This function
#' uses an API offered by Wikipedia to extract the URL of that image.
#'
#' @param taxa character with the taxa for which an image URL should be
#'  obtained.
#' @param size integer giving the width of the image in pixel.
#'
#' @return
#' a character vector with URLs. For taxa where no thumbnail was found, the
#' function returns `NA`.
#'
#' @export

get_wikipedia_image_urls <- function(taxa, size = 100) {

  # make sure that size is an integer
  int_size <- suppressWarnings(as.integer(size))
  if (is.na(int_size) || size <= 0) {
    cli::cli_abort("{size} is not a positive integer.")
  }

  vapply(taxa, get_wikipedia_image_url, character(1), size = int_size)

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
