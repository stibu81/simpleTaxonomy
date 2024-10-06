#' Find Taxons By Pattern
#'
#' Find taxony by pattern and return their names. The output can be passed
#' to the arguments `show`, `full_expand` or `focus` in [`plot_taxonomy()`].
#'
#' @param data tibble defining the taxonomic hierarchy, typically created with
#' [`read_taxonomy()`].
#' @param pattern a regex pattern
#' @param target character giving the column to search in
#' ("name" or "scientific"). The default is "all" which searches in all columns.
#'
#' @return
#' a character vector giving the taxon names that match the pattern
#'
#' @export

find_taxon <- function(data, pattern, target = c("all", "name", "scientific")) {

  target <- match.arg(target)

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
