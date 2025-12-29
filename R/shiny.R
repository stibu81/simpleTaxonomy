#' Run simpleTaxonomy Shiny App
#'
#' Run the simple shiny app that is included with simpleTaxonomy. The app
#' provides several features to interact with a `taxonomy_graph`.
#' A `taxonomy_graph` can be read from a file or an URL. By default, the
#' app loads the data from <https://github.com/stibu81/taxonomyData>.
#'
#' @param file Path to a file containing a `taxonomy_graph`. If omitted, the
#' data from <https://github.com/stibu81/taxonomyData> is used.
#' @param expand_ranks a list of ranks that should always be expanded. This
#' can be changed interactively in the app.
#' @param image_size numeric giving the default image width in pixels used in
#' the app. The user can change this interactively in the app.
#' @param link_length numeric giving the default length of links in pixels
#' used in the app. The user can change this interactively in the app.
#' @param launch_browser logical, if \code{TRUE}, the application
#' is opened in the system's default browser, if \code{FALSE},
#' no browser is started. If the argument is omitted, the value
#' according to the option \code{shiny.launch.browser} is used,
#' which in RStudio opens the internal shiny viewer.
#'
#' @export

run_taxonomy <- function(file = NULL,
                         expand_ranks = c("Gattung", "Art", "Unterart"),
                         image_size = 150,
                         link_length = 200,
                         launch_browser = NULL) {

  rlang::check_installed(c("shiny", "bslib", "DT"),
                         "in order to run the app.")

  app_dir <- system.file("shinyApp", package = "simpleTaxonomy")
  if (app_dir == "") {
    cli::cli_abort(
      "Could not find the shiny app. Try re-installing \"simpleTaxonomy\"."
    )
  }

  # pass settings as options to the app
  options(
    simpleTaxonomy_file = if (is.null(file)) file else normalizePath(file),
    simpleTaxonomy_expand_ranks = expand_ranks,
    simpleTaxonomy_image_size = image_size,
    simpleTaxonomy_link_length = link_length
  )

  if (is.null(launch_browser)) {
    launch_browser <- getOption("shiny.launch.browser", interactive())
  }

  shiny::runApp(app_dir,
                display.mode = "normal",
                launch.browser = launch_browser)
}
