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


create_counts_dt <- function(taxonomy,
                             root,
                             by_rank,
                             only_major_ranks,
                             show_all) {
  if (root != "") {
      by_rank <- if (!by_rank %in% c("", "ohne")) by_rank
      # catch error in count_ranks() and return empty tibble in case of error
      # not catching this error leads to an error message briefly flashing
      # in some situations.
      rank_counts <- tryCatch(
        count_ranks(
          taxonomy,
          subgraph = root,
          by_rank = by_rank,
          only_major_ranks = only_major_ranks
        ),
        error = function(e) dplyr::tibble()
      )

      # set the number of rows to show. Only show the pagination-controls
      # (dom = "p) if there are multiple pages.
      n_rows <- if (show_all) 1000 else 12
      dom <- if (nrow(rank_counts) > n_rows) "ftp" else "ft"

      # if there is no summary by rank, use better column names
      if (is.null(by_rank)) {
        names(rank_counts) <- c("Rangstufe", "Anzahl")
      }

      DT::datatable(
        rank_counts,
        rownames = FALSE,
        selection = "none",
        extensions = "FixedColumns",
        options = list(
          dom = dom,
          pageLength = n_rows,
          language  = list(
            search = "<b>Suche:</b>",
            emptyTable = "Es sind keine Daten verf\u00fcgbar.",
            paginate = list(previous = "Zur\u00fcck", `next` = "N\u00e4chste")
          ),
          scrollX = TRUE,
          fixedColumns = list(leftColumns = 1)
        )
      )
    }
}

create_wiki_button <- function(taxonomy, taxon) {
  if (taxon == "") return(NULL)

  taxon <- get_taxon_names(taxonomy, taxon)
  i_taxon <- names(igraph::V(taxonomy)) == taxon
  sci <- igraph::vertex_attr(taxonomy, "scientific")[i_taxon]
  link <- paste0(
    "https://de.wikipedia.org/wiki/",
    stringr::str_replace_all(if (is.na(sci)) taxon else sci, " +", "_")
  )
  shiny::actionButton(
    inputId = "wiki_button",
    class = "btn-primary btn-rounded",
    label = taxon,
    icon = shiny::icon("wikipedia-w"),
    onclick = paste0("window.open(\"", link, "\", \"_blank\")")
  )
}
