#' Create an svg Flag Icon Tag
#'
#' Create an html `<img>` tag for a country flag svg file that is shipped with
#' the package. The flag icons are by [Lipis](https://github.com/lipis) and were
#' downloaded from <https://github.com/lipis/flag-icons>.
#'
#' @param code character with the flag code, e.g., `"ch"` or `"de"`.
#'  To find available flag codes, use [`get_flag_info()`].
#' 
#' @export

flag_icon <- function(code) {
  
  if (!is.character(code) || length(code) != 1 || is.na(code)) {
    cli::cli_abort("code must be a single character value.")
  }

  code <- tolower(trimws(code))
  if (code == "") {
    cli::cli_abort("code must not be empty.")
  }

  flag_file <- system.file(
    "flags", "svg", paste0(code, ".svg"),
    package = "simpleTaxonomy"
  )
  if (flag_file == "") {
    cli::cli_abort(
      c(
        "!" = "Flag with code {.val {code}} was not found.",
        "i" = paste(
          "Use {.run [get_flag_info()](simpleTaxonomy::get_flag_info())}",
          "to find available flag codes."
        )
      )
    )
  }

  rlang::check_installed("base64enc")
  shiny::img(
    src = base64enc::dataURI(file = flag_file, mime = "image/svg+xml"),
    class = glue::glue("flag flag-{code}")
  )
}


#' Get Information About the Available Flag Codes
#' 
#' Return a table that for all available flags returns the country name,
#' the continent, the flag code and the capital. The flag code must be used
#' as input in functions like [`flag_icon()`] and [`run_taxonomy()`].
#' 
#' @param filter a regex pattern that is used to search in the country names.
#'  Search is case-insensitive.
#' 
#' @returns
#' a tibble with character columns `name`, `continent`, `code`, and `capital`.
#' 
#' @export

get_flag_info <- function(filter = NULL) {
  country_data <- system.file(
      "flags", "country.json",
      package = "simpleTaxonomy"
    ) %>% 
    jsonlite::fromJSON() %>% 
    dplyr::as_tibble()

  # apply filter
  if (!is.null(filter)) {
    pattern <- stringr::regex(filter, ignore_case = TRUE)
    country_data <- country_data %>% 
      dplyr::filter(stringr::str_detect(.data$name, pattern))
  }

  country_data
}