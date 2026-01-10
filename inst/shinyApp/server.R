function(input, output, session) {

  taxonomy_sg <- reactive({
    if (input$tree_root != "") {
      logger::log_info("filter the graph to the root '{input$tree_root}'")
      get_subgraph(taxonomy, input$tree_root)
    }
  })

  # choices of taxa_show and counts_root must be filled here in order
  # to use server side processing
  updateSelectizeInput(
    session,
    "tree_root",
    choices = no_leaf_taxa,
    selected = names(get_root_node(taxonomy)),
    server = TRUE
  )
  # this input mut only contain taxa that are present in the taxonomy_graph
  # from the previously selected values, only those are kept that are still
  # valid
  observe({
    # only do the update, if we are working with a valid taxonomy_graph
    if (!is.null(taxonomy_sg())) {
      logger::log_info("update choices in input$taxa_show")
      selected <- isolate(input$taxa_show)
      choices <- names(attr(taxonomy_sg(), "match_labs"))
      selected_new <- intersect(selected, choices)
      freezeReactiveValue(input, "taxa_show")
      updateSelectizeInput(
        session,
        "taxa_show",
        choices = choices,
        selected = selected_new,
        server = TRUE
      )
    }
  })
  updateSelectizeInput(
    session,
    "counts_root",
    choices = no_leaf_taxa,
    server = TRUE
  )

  output$taxonomy_plot <- collapsibleTree::renderCollapsibleTree({

    if (is.null(taxonomy_sg())) {
      logger::log_info("plotting an empty graph")
      return(NULL)
    }

    image_size <- debounce(reactive(input$image_size), 500)
    link_length <- debounce(reactive(input$link_length), 500)

    logger::log_info(
      "plotting graph with root '{names(get_root_node(taxonomy_sg()))}'"
    )
    # in some situations, calling input$taxa_show fails with an error that
    # I don't understand. To be safe, I use a tryCatch block to evaluate it.
    str_taxa_show <- tryCatch(
      paste(input$taxa_show, collapse = '\', \''),
      error = function(e) ""
    )
    logger::log_info("plot shows the following taxa: '{str_taxa_show}'")

    plot_taxonomy(taxonomy_sg(),
                  show = input$taxa_show,
                  full_expand = if (input$full_expand) input$taxa_show,
                  highlight = if (input$highlight) input$taxa_show,
                  expand_rank = input$expand_ranks,
                  show_images = input$show_images,
                  link_length = link_length(),
                  image_size = image_size())
  })

  output$wikipedia_link <- renderUI({
    # if no root taxon is currently selected, fall back to the root of the
    # unfiltered graph.
    clicked_taxon <- if (is.null(taxonomy_sg())) {
      names(get_root_node(taxonomy))
    # at the start, input$select_taxon is NULL. When the root is clicked,
    # it is an empty list. => In both cases, show the link for the root taxon.
    } else if (length(input$selected_taxon) == 0) {
      names(get_root_node(taxonomy_sg()))
    # if any other node is clicked, input$select_taxon is a list containing
    # the labels of oll the nodes from the root to the clicked node.
    # => take the last taxon in the list.
    } else {
      unlist(tail(input$selected_taxon, n = 1))
    }
    simpleTaxonomy:::create_wiki_button(taxonomy, clicked_taxon)
  })

  # fill the selection of orders to group by according to the selection
  # of the top level taxon
  observeEvent(
    input$counts_root,
    {
      if (input$counts_root != "") {
        logger::log_info("update input$counts_by_rank")
        old_by_rank_value <- input$counts_by_rank
        subgraph <- get_subgraph(taxonomy, input$counts_root)
        use_ranks <- c("ohne",
                       intersect(ranks, igraph::vertex_attr(subgraph, "rank")))
        # if the old value is still valid, keep it, otherwise select "ohne"
        new_by_rank_value <- if (old_by_rank_value %in% use_ranks) {
          old_by_rank_value
          } else {
            "ohne"
          }
        freezeReactiveValue(input, "counts_by_rank")
        updateSelectizeInput(
          session,
          "counts_by_rank",
          choices = use_ranks,
          selected = new_by_rank_value
        )
      }
    }
  )

  output$rank_counts <- DT::renderDT({
    logger::log_info("compute rank counts for root '{input$counts_root}'")
    simpleTaxonomy:::create_counts_dt(taxonomy,
                                      input$counts_root,
                                      input$counts_by_rank,
                                      input$only_major_ranks,
                                      input$counts_show_all)
  })

  output$counts_image <- renderUI({
    if (input$counts_root == "") return(NULL)
    taxon_node <- igraph::induced_subgraph(
      taxonomy,
      get_taxon_names(taxonomy, input$counts_root)
    )
    tooltip <- simpleTaxonomy:::create_tooltip(taxon_node, TRUE, 220)
    HTML(tooltip)
  })

  output$counts_wikipedia_link <- renderUI(
    simpleTaxonomy:::create_wiki_button(taxonomy, input$counts_root)
  )

}
