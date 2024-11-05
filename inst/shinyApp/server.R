library(shiny)
library(dplyr)
library(stringr)
library(collapsibleTree)
library(simpleTaxonomy)


function(input, output, session) {

  # choices of taxa_show must be filled here in order to use server side
  # processing
  updateSelectizeInput(
    session,
    "taxa_show",
    choices = names(taxa),
    server = TRUE
  )

  output$taxonomy_plot <- renderCollapsibleTree({

    image_size <- debounce(reactive(input$image_size), 500)

    plot_taxonomy(taxonomy,
                  show = taxa[input$taxa_show],
                  full_expand = if (input$full_expand) taxa[input$taxa_show],
                  highlight = if (input$highlight) taxa[input$taxa_show],
                  expand_rank = input$expand_ranks,
                  show_images = input$show_images,
                  image_size = image_size())
  })

  output$wikipedia_link <- renderUI({
    if (length(input$selected_taxon) > 0) {
      current_taxon <- tail(input$selected_taxon, n = 1)
      link <- paste0("https://de.wikipedia.org/wiki/",
                     str_replace_all(current_taxon, " +", "_"))
      shiny::actionButton(
        inputId = "wiki_button",
        class = "btn-primary",
        label = current_taxon,
        icon = icon("wikipedia-w"),
        onclick = paste0("window.open(\"", link, "\", \"_blank\")")
      )
    }
  })

}
