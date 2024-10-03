#' Read Taxonomic Hierarchy From File
#'
#' Read a csv file that contains the data that defines a taxonomic
#' hierarchy.
#'
#' @param file path to the csv file
#' @param delim the delimiter used in the file
#'
#' @details
#' The file must contain column names in the first row. Each row defines a
#' taxon with the following attributes:
#'
#' \describe{
#' \item{parent}{The name of the parent taxon. This column must be empty for
#' the root taxon.}
#' \item{name}{The name of the taxon that should be used to label the nodes,
#' e.g., the common name. These names must be unique.}
#' \item{scientific}{The scientific name of the taxon.}
#' \item{rank}{The rank of the taxon, e.g., "Familie", "Art", or similar (only
#' German ranks are supported so far).
#' These will be used to colour the nodes.}
#' }
#'
#' The function checks that the file satisfies the following conditions:
#' * The required columns are all present.
#' * There is exactly one root taxon, which is a taxon without parent.
#' * There are no duplicated names.
#' * Each parent taxon is defined by its own row in the table.
#' * The rank is always defined.
#'
#' @return
#' a tibble that can be used to create a visualisation of a
#' taxonomic hierarchy.
#'
#' @export

read_taxonomy <- function(file, delim = ",") {

  data <- readr::read_delim(file, delim = delim, col_types = "c")

  # check columns and select the required columns
  expected_names <- c("parent", "name", "scientific", "rank")
  missing_names <- setdiff(expected_names, names(data))
  if (length(missing_names) > 0) {
    cli::cli_abort(
      paste("The following required columns are missing from {file}:",
            "\"{paste(missing_names, collapse = '\", \"')}\"")
    )
  }
  data <- data %>%
    dplyr::select(dplyr::all_of(expected_names))

  # check unique root taxon
  root <- data$name[is.na(data$parent)]
  if (length(root)  == 0) {
    cli::cli_abort("{file} does not define a root taxon.")
  }
  if (length(root) > 1) {
    cli::cli_abort(
      paste("{file} contains multiple root taxons:",
            "\"{paste(root, collapse = '\", \"')}\"")
    )
  }

  # name must not be missing and unique
  i_missing_name <- which(is.na(data$name))
  if (length(i_missing_name) > 0) {
    cli::cli_abort(
      paste("{file} has missing name in row(s)",
            paste(i_missing_name + 1, collapse = ", "))
    )
  }
  dup_names <- unique(data$name[duplicated(data$name)])
  if (length(dup_names) > 0) {
    cli::cli_abort(
      paste("{file} has duplicate names:",
            "\"{paste(dup_names, collapse = '\", \"')}\"")
    )
  }

  # all parent taxons must be properly defined
  missing_parents <- setdiff(data$parent, c(data$name, NA_character_))
  if (length(missing_parents) > 0) {
    cli::cli_abort(
      paste("{file} has undefined parent taxons:",
            "\"{paste(missing_parents, collapse = '\", \"')}\"")
    )
  }

  # rank must not be missing
  i_missing_rank <- which(is.na(data$rank))
  if (length(i_missing_rank) > 0) {
    cli::cli_abort(
      paste("{file} has missing rank in row(s)",
            paste(i_missing_rank + 1, collapse = ", "))
    )
  }

  data
}
