#' Plot a Taxonomic Hierarchy
#'
#' Create an interactive visualisation of a taxonomic hierarchy.
#'
#' @param data tibble defining the taxonomic hierarchy, typically created with
#' [`read_taxonomy()`].
#' @param font_size integer giving the font size of the labels in pixels.
#'
#' @return
#' a htmlwidget with the interactive tree visualisation.
#'
#' @export

plot_taxonomy <- function(data, font_size = 12) {

  # add columns for colour
  colours <- c(
    "Lebewesen" = "#104E8B",
    "Dom\u00e4ne" = "#1C86EE",
    "Reich" = "#00FFFF",
    "Stamm" = "#76EEC6",
    "Unterstamm" = "#66CDAA",
    "Klasse" = "#00CC00",
    "Ordnung" = "#33FF33",
    "\u00dcberfamilie" = "#CCF90D",
    "Familie" = "#FFF700",
    "Unterfamilie" = "#FFC600",
    "Tribus" = "#FFA500",
    "Gattung" = "#CD8500",
    "Art"= "#A52A2A"
  )
  data$colour <- unname(colours[data$rank])

  # add tooltip
  data$tooltip <- paste0(
    data$rank, "</br>",
    "<strong>", data$name, "</strong></br>",
    "(", data$scientific, ")"
  )

  collapsibleTree::collapsibleTreeNetwork(
    data,
    fill = "colour",
    tooltipHtml = "tooltip",
    fontSize = font_size
  )

}
