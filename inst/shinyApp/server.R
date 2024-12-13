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
    link_length <- debounce(reactive(input$link_length), 500)

    plot_taxonomy(taxonomy,
                  show = taxa[input$taxa_show],
                  full_expand = if (input$full_expand) taxa[input$taxa_show],
                  highlight = if (input$highlight) taxa[input$taxa_show],
                  expand_rank = input$expand_ranks,
                  show_images = input$show_images,
                  link_length = link_length(),
                  image_size = image_size())
  })

  output$wikipedia_link <- renderUI({
    # at the start, input$select_taxon is NULL. When the root is clicked,
    # it is an empty list. => In both cases, show the link for the root taxon.
    clicked_taxon <- if (length(input$selected_taxon) == 0) {
      names(get_root_node(taxonomy))
    # if any other node is clicked, input$select_taxon is a list containing
    # the labels of oll the nodes from the root to the clicked node.
    # => take the last taxon in the list.
    } else {
      tail(input$selected_taxon, n = 1)
    }
    link <- paste0("https://de.wikipedia.org/wiki/",
                   str_replace_all(clicked_taxon, " +", "_"))
    shiny::actionButton(
      inputId = "wiki_button",
      class = "btn-primary btn-rounded",
      label = clicked_taxon,
      icon = icon("wikipedia-w"),
      onclick = paste0("window.open(\"", link, "\", \"_blank\")")
    )
  })

}
