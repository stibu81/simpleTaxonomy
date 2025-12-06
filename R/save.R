#' Save Taxonomic Hierarchy to HTML-File
#'
#' Save a plot of a taxonomic hierarchy created with [`plot_taxonomy()`] to
#' a self-contained HTML-file.
#'
#' @param widget a widget created with [`plot_taxonomy()`].
#' @param file the full path to the file
#' @param title text to use as the title of the saved page
#' @param background character giving the html background colour of
#'  the widget. Defaults to white.
#'
#' @export

save_taxonomy <- function(widget, file,
                          background = "white",
                          title = "simpleTaxonomy") {

  # prepare the css
  css_file <- system.file("shinyApp", "www", "simpleTaxonomy.css",
                          package = "simpleTaxonomy")
  css <- readr::read_lines(css_file) %>%
    paste(collapse = "\n")

  widget %>%
    htmlwidgets::prependContent(htmltools::tags$style(css)) %>%
    htmlwidgets::saveWidget(file, background = background, title = title)
}
