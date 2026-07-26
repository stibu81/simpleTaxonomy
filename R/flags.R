#' Create an svg Flag Icon Tag
#'
#' Create an html `<img>` tag for a country flag svg file that is shipped with
#' the package. The flag icons are by [Lipis](https://github.com/lipis) and were
#' downloaded from <https://github.com/lipis/flag-icons>.
#'
#' @param code character with the flag code, e.g., `"ch"` or `"de"`.
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
    cli::cli_abort("Flag with code {.val {code}} was not found in package assets.")
  }

  rlang::check_installed("base64enc")
  shiny::img(
    src = base64enc::dataURI(file = flag_file, mime = "image/svg+xml"),
    class = glue::glue("flag flag-{code}")
  )
}