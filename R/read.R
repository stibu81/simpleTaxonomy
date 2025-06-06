#' Read Taxonomic Hierarchy From File
#'
#' Read a csv file that contains the data that defines a taxonomic
#' hierarchy and return it as a graph.
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
#' In addition, the function also warns if an unknown rank is used. This will
#' still lead to a graph that can be plotted, but the nodes with unknown rank
#' will not be coloured.
#'
#' The common names (column "name") of the taxa must be unique, because they
#' are used to create the graph. Sometimes, there are taxa that have identical
#' common names, e.g. the family Equidae and the genus Equus are both called
#' "Pferde" in German. In theses cases, one can use an additional identifier
#' in parenthesis to make the names unique, e.g., "Pferde (F)" and "Pferde" for
#' the family and the genus, respectively. The identifier "(F)" will be removed
#' and not be shown in the visualisation.
#'
#' @return
#' a `taxonomy_graph` object which inherits from `igraph`
#'
#' @examples
#' file <- get_example_taxonomy_file()
#' taxonomy <- read_taxonomy(file)
#' class(taxonomy)
#' as_tibble(taxonomy)
#'
#' @export

read_taxonomy <- function(file, delim = ",") {

  data <- file %>%
    read_taxonomy_file(delim) %>%
    clean_taxonomy_df() %>%
    check_taxonomy_df() %>%
    prepare_taxonomy_df()

  create_taxonomy_graph(data)

}


read_taxonomy_file <- function(file, delim, error_call = rlang::caller_env()) {

  # only check if file exists for local paths, not for urls
  if (!is_url(file) && !file.exists(file)) {
    cli::cli_abort("file \"{file}\" does not exist.", call = error_call)
  }

  readr::read_delim(file, delim = delim, col_types = "c")
}

# Perform a few cleaning steps on the taxonomy data
clean_taxonomy_df <- function(data) {

  data %>%
    # trim white space, fix repeated white space
    dplyr::mutate(
      dplyr::across(dplyr::everything(), stringr::str_squish)
    ) %>%
    # remove empty rows
    dplyr::filter(
      dplyr::if_any(dplyr::everything(), ~!is.na(.x))
    )

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

  # if the column "image_url" is missing, add it and fill with NA
  if (!"image_url" %in% names(data)) {
    data <- data %>%
      dplyr::mutate(image_url = NA_character_)
  }

  data <- data %>%
    dplyr::select(dplyr::all_of(expected_names), "image_url")

  # check unique root taxon
  root <- data$name[is.na(data$parent)]
  if (length(root)  == 0) {
    cli::cli_abort("There is no root taxon.", call = error_call)
  }
  if (length(root) > 1) {
    cli::cli_abort(
      paste("There are multiple root taxa:",
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

  # also scientific names must be unique
  dup_sci <- unique(data$scientific[duplicated(data$scientific)])
  if (length(dup_sci) > 0) {
    cli::cli_abort(
      paste("There are duplicate scientific names:",
            "\"{paste(dup_sci, collapse = '\", \"')}\""),
      call = error_call
    )
  }

  # all parent taxa must be properly defined
  missing_parents <- setdiff(data$parent, c(data$name, NA_character_))
  if (length(missing_parents) > 0) {
    cli::cli_abort(
      paste("There are undefined parent taxa:",
            "\"{paste(missing_parents, collapse = '\", \"')}\""),
      call = error_call
    )
  }

  # rank must not be missing
  missing_rank <- data$name[is.na(data$rank)]
  if (length(missing_rank) > 0) {
    cli::cli_abort(
      paste("Some taxa have no rank:",
            "\"{paste(missing_rank, collapse = '\", \"')}\""),
      call = error_call
    )
  }

  data

}


# Prepare taxonomy df with additional columns that are needed for
# the visualisation
prepare_taxonomy_df <- function(data) {

  # the names may contain additional identifiers in parenthesis to distinguish
  # taxa that have otherwise indistinguishable common names. Remove these
  # for the label in the graph
  data <- data %>%
    dplyr::mutate(
      label = stringr::str_remove(.data$name, "\\(.*\\)") %>%
        stringr::str_trim()
    )

  data <- data %>%
    dplyr::left_join(get_rank_colours(), by = "rank")

  # warn if there are invalid ranks
  invalid_ranks <- unique(data$rank[is.na(data$colour)])
  if (length(invalid_ranks) > 0) {
    cli::cli_alert_warning(
      paste("There are invalid ranks: ",
            "\"{paste(invalid_ranks, collapse = '\", \"')}\".",
            "The corresponding nodes will not be coloured.")
    )
  }

  # by default, collapse everything
  data$collapsed <- TRUE

  data

}


#' Get a Table of Available Ranks
#'
#' Get the available ranks as a data frame. The ranks are returned in the
#' correct hierarchical order from highest to lowest. Other ranks may be used,
#' but only those listed here will be represented by coloured nodes in the
#' visualisation.
#'
#' @returns
#' a tibble with columns `level` and `de` (ranks in German).
#'
#' @export

available_ranks <- function() {
  get_rank_colours() %>%
    dplyr::mutate(
      level = c(1:(dplyr::n() - 1), NA_integer_)
    ) %>%
    dplyr::select("level", de = "rank")
}


# get a data frame with the colours by rank
get_rank_colours <- function() {

  dplyr::tribble(
    ~"rank",              ~"colour",
    "Lebewesen",          "#3333CC",
    "Dom\u00e4ne",        "#0B55AE",
    "Reich",              "#12B5F4",
    "Stamm",              "#00FFFF",
    "Unterstamm",         "#00CCCC",
    "Teilstamm",          "#00AA86",
    "\u00dcberklasse",    "#006600",
    "Klasse",             "#008000",
    "Unterklasse",        "#0FA601",
    "Teilklasse",         "#16B701",
    "Kohorte",            "#9900CC",
    "Unterkohorte",       "#CC00CC",
    "Division",           "#FF00FF",
    "Serie",              "#FF99FF",
    "\u00dcberordnung",   "#24D902",
    "Ordnung",            "#33FF33",
    "Unterordnung",       "#9EFB18",
    "Teilordnung",        "#B3FA13",
    "\u00dcberfamilie",   "#EBF805",
    "Familie",            "#FFF700",
    "Unterfamilie",       "#FFC600",
    "Tribus",             "#FFA500",
    "Gattung",            "#CD8500",
    "Art",                "#A52A2A",
    "Unterart",           "#7C2020",
    "Reihe",              "#FFFFFF",
    "ohne Rang",          "#FFFFFF"
  )
}


#' Get Example Taxonomy File
#'
#' Return the path to an example file containing a partial taxonomic hierarchy
#' of the order Carnivora.
#' It can be read with [read_taxonomy()] and plotted with [plot_taxonomy()].
#'
#' @export

get_example_taxonomy_file <- function() {
  system.file("example", "carnivora.csv", package = "simpleTaxonomy")
}


# check whether a path is a URL
is_url <- function(path) {
  stringr::str_detect(path, "^https?://")
}
