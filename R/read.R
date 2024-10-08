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

  data <- readr::read_delim(file, delim = delim, col_types = "c") %>%
    check_taxonomy_df()

  data

}


# Check validity of the data read from the csv file and produce
# helpful messages
check_taxonomy_df <- function(data, error_call = rlang::caller_env()) {

  # check columns and select the required columns
  expected_names <- c("parent", "name", "scientific", "rank")
  missing_names <- setdiff(expected_names, names(data))
  if (length(missing_names) > 0) {
    cli::cli_abort(
      paste("The following required columns are missing:",
            "\"{paste(missing_names, collapse = '\", \"')}\""),
      call = error_call
    )
  }
  data <- data %>%
    dplyr::select(dplyr::all_of(expected_names))

  # check unique root taxon
  root <- data$name[is.na(data$parent)]
  if (length(root)  == 0) {
    cli::cli_abort("There is no root taxon.", call = error_call)
  }
  if (length(root) > 1) {
    cli::cli_abort(
      paste("There are multiple root taxons:",
            "\"{paste(root, collapse = '\", \"')}\""),
      call = error_call
    )
  }

  # name must not be missing and unique
  i_missing_name <- which(is.na(data$name))
  if (length(i_missing_name) > 0) {
    cli::cli_abort(
      paste("There are missing names in row(s):",
            paste(i_missing_name + 1, collapse = ", ")),
      call = error_call
    )
  }
  dup_names <- unique(data$name[duplicated(data$name)])
  if (length(dup_names) > 0) {
    cli::cli_abort(
      paste("There are duplicate names:",
            "\"{paste(dup_names, collapse = '\", \"')}\""),
      call = error_call
    )
  }

  # all parent taxons must be properly defined
  missing_parents <- setdiff(data$parent, c(data$name, NA_character_))
  if (length(missing_parents) > 0) {
    cli::cli_abort(
      paste("There are undefined parent taxons:",
            "\"{paste(missing_parents, collapse = '\", \"')}\""),
      call = error_call
    )
  }

  # rank must not be missing
  missing_rank <- data$name[is.na(data$rank)]
  if (length(missing_rank) > 0) {
    cli::cli_abort(
      paste("Some taxons have no rank:",
            "\"{paste(missing_rank, collapse = '\", \"')}\""),
      call = error_call
    )
  }

  data

}
