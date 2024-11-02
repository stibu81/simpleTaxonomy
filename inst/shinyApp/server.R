library(shiny)
library(dplyr)
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

    plot_taxonomy(taxonomy,
                  show = taxa[input$taxa_show],
                  full_expand = if (input$full_expand) taxa[input$taxa_show],
                  expand_rank = input$expand_ranks,
                  show_images = input$show_images)
  })

}
