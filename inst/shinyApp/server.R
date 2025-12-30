library(shiny)
library(dplyr)
library(stringr)
library(DT)
library(igraph)
library(collapsibleTree)
library(simpleTaxonomy)


function(input, output, session) {

  # choices of taxa_show and counts_root must be filled here in order
  # to use server side processing
  updateSelectizeInput(
    session,
    "taxa_show",
    choices = names(taxa),
    server = TRUE
  )
  updateSelectizeInput(
    session,
    "counts_root",
    choices = names(no_leaf_taxa),
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
    create_wiki_button(taxonomy, clicked_taxon)
  })

  # fill the selection of orders to group by according to the selection
  # of the top level taxon
  observeEvent(
    input$counts_root,
    {
      if (input$counts_root != "") {
        old_by_rank_value <- input$counts_by_rank
        subgraph <- get_subgraph(taxonomy, no_leaf_taxa[input$counts_root])
        use_ranks <- c("ohne", intersect(ranks, vertex_attr(subgraph, "rank")))
        # if the old value is still valid, keep it, otherwise select "ohne"
        new_by_rank_value <- if (old_by_rank_value %in% use_ranks) {
          old_by_rank_value
          } else {
            "ohne"
          }
        updateSelectizeInput(
          session,
          "counts_by_rank",
          choices = use_ranks,
          selected = new_by_rank_value
        )
      }
    }
  )

  output$rank_counts <- renderDataTable(
    simpleTaxonomy:::create_counts_dt(taxonomy,
                                      input$counts_root,
                                      input$counts_by_rank,
                                      input$only_major_ranks,
                                      input$counts_show_all,
                                      no_leaf_taxa)
  )

  output$counts_image <- renderUI({
    i_taxon <- which(names(V(taxonomy)) == no_leaf_taxa[input$counts_root])
    taxon_node <- induced_subgraph(taxonomy, i_taxon)
    tooltip <- simpleTaxonomy:::create_tooltip(taxon_node, TRUE, 220)
    HTML(tooltip)
  })

  output$counts_wikipedia_link <- renderUI(
    simpleTaxonomy:::create_wiki_button(taxonomy, input$counts_root)
  )

}
