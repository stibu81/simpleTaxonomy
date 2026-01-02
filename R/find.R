#' Find Taxa By Pattern
#'
#' Find taxa by pattern and return their common names. The output can be passed
#' to the arguments `show`, `full_expand` or `focus` in [`plot_taxonomy()`].
#'
#' @param graph a `taxonomy_graph` object, typically created with
#' [`read_taxonomy()`].
#' @param pattern a regex pattern, matching is case insensitive.
#' @param target character giving the column to search in
#' ("name" or "scientific"). The default is "all" which searches in all columns.
#'
#' For more efficient exact matching, use [`get_taxon_names()`].
#'
#' @return
#' a character vector giving the taxon names that match the pattern
#'
#' @examples
#' file <- get_example_taxonomy_file()
#' taxonomy <- read_taxonomy(file)
#' find_taxon(taxonomy, "katze")
#' find_taxon(taxonomy, "felis")
#'
#' @seealso get_taxon_names
#'
#' @export

find_taxon <- function(graph,
                       pattern,
                       target = c("all", "name", "scientific")) {

  target <- match.arg(target)

  data <- igraph::vertex.attributes(graph)

  # ignore case
  pattern <- stringr::regex(pattern, ignore_case = TRUE)

  match_name <- if (target %in% c("name", "all")) {
    stringr::str_detect(data$name, pattern)
  } else {
    FALSE
  }

  match_sci <- if (target %in% c("scientific", "all")) {
    stringr::str_detect(data$scientific, pattern)
  } else {
    FALSE
  }

  data$name[match_name | match_sci] %>%
    Filter(f = \(x) !is.na(x))
}


#' Get Taxon Labels by Common or Scientific Name
#'
#' The Taxa in the `taxonomy_graph` are labelled by their common name.
#' This function returns the label for a vector of common and/or scientific
#' names. It is much more efficient than [`find_taxon()`] because it relies
#' on a matching table stored in the `taxonomy_graph`.
#'
#' @param taxonomy a `taxonomy_graph` object, typically created with
#' [`read_taxonomy()`].
#' @param char a character vector of common and/or scientific names to be
#'  converted to labels. Matching is case-sensitive.
#'
#' @returns
#' a character vector of the same length as `char` containing taxon labels.
#'
#' @seealso find_taxon
#'
#' @export

get_taxon_names <- function(taxonomy, char) {
  attr(taxonomy, "match_labs")[char]
}
