sidebar_tree <- sidebar(
  width = 270,
  gap = 15,
  padding = c(15, 10),
  bg = "#F2F2F2",
  # place drop-down for selection of root and button for going to parent next
  # to each other.
  # the label for the selectizeInput() must be defined independently to ensure
  # proper vertical alignment of the selectize and the button
  tags$label(`for` = "tree_root", "Wurzel-Taxon:"),
  simpleTaxonomy:::flex_row(
    # choices will be filled in the server in order to use server side
    # processing
    selectizeInput(
      "tree_root",
      label = NULL,
      choices = NULL,
      multiple = FALSE
    ),
    actionButton(
      "tree_to_parent",
      label = NULL,
      icon = icon("up-long"),
      title = "Zum Elterntaxon",
      class = "btn-primary btn-rounded",
      width = 38
    )
  ),
  selectizeInput(
    "taxa_show",
    "Taxa anzeigen:",
    choices = NULL,
    multiple = TRUE
  ),
  input_switch(
    "full_expand",
    label = "Vollständig öffnen",
    value = FALSE
  ),
  input_switch(
    "highlight",
    label = "Hervorheben",
    value = FALSE
  ),
  # choices are filled here to avoid repeated rendering of the plot when
  # the app starts
  selectizeInput(
    "expand_ranks",
    label = "Stufen immer öffnen:",
    choices = available_ranks()[["de"]],
    selected = expand_ranks_default,
    multiple = TRUE
  ),
  uiOutput("wikipedia_link"),
  accordion(
    accordion_panel(
      "Darstellung",
      input_switch(
        "show_images",
        label = "Bilder anzeigen",
        value = TRUE
      ),
      input_switch(
        "highlight_missing_images",
        label = "Fehlende Bilder hervorheben",
        value = FALSE
      ),
      sliderTextInput(
        "image_size",
        label = "Bildgrösse:",
        choices = c("60", "120", "250", "330", "500"),
        selected = image_size_default,
        grid = TRUE
      ),
      sliderInput(
        "link_length",
        label = "Verbindungslänge:",
        min = 100, max = 500,
        step = 10,
        value = link_length_default
      )
    ),
    open = FALSE
  )
)

sidebar_counts <- sidebar(
  width = 270,
  gap = 15,
  padding = c(15, 10),
  bg = "#F2F2F2",
  # place drop-down for selection of root and button for going to parent next
  # to each other.
  # the label for the selectizeInput() must be defined independently to ensure
  # proper vertical alignment of the selectize and the button
  tags$label(`for` = "counts_root", "Zähle Taxa in:"),
  simpleTaxonomy:::flex_row(
    # choices will be filled in the server in order to use server side
    # processing
    selectizeInput(
      "counts_root",
      label = NULL,
      choices = NULL,
      multiple = FALSE
    ),
    actionButton(
      "counts_to_parent",
      label = NULL,
      icon = icon("up-long"),
      title = "Zum Elterntaxon",
      class = "btn-primary btn-rounded",
      width = 38
    )
  ),
  input_switch(
    "only_major_ranks",
    label = "Nur Hauptrangstufen",
    value = FALSE
  ),
  selectizeInput(
    "counts_by_rank",
    "Gruppieren nach Rang:",
    choices = "ohne",
    selected = "ohne"
  ),
  input_switch(
    "counts_show_all",
    label = "Alle anzeigen",
    value = FALSE
  ),
  tags$strong("Ausgewähltes Taxon:"),
  # use inline for better spacing.
  htmlOutput("counts_image", inline = TRUE),
  uiOutput("counts_wikipedia_link")
)

page_navbar(

  title = h4("simpleTaxonomy",
             img(src = "simpleTaxonomy_logo.png",
                 style = "position:absolute; right:20px; top:5px",
                 height = 42)),

  # use the colour from the logo as primary colour, slightly darkened for better
  # contrast with colorspace::darken("turquoise3", amount = 0.07)
  theme = bs_theme(preset = "yeti", primary = "#06b6bd"),

  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "simpleTaxonomy.css"),
    tags$link(rel="icon", href="favicon-16x16.png"),
  ),

  nav_item(
    actionButton(
      "reload",
      label = NULL,
      icon = icon("rotate-right"),
      class = "nav-link",
      title = "Neu laden"
    )
  ),

  nav_panel(
    "Baum",
    layout_sidebar(
      sidebar = sidebar_tree,
      card(collapsibleTree::collapsibleTreeOutput("taxonomy_plot"))
    )
  ),

  nav_panel(
    "Tabelle",
    layout_sidebar(
      sidebar = sidebar_counts,
      card(DT::DTOutput("rank_counts"))
    )
  ),

  navbar_options = navbar_options(
    theme = "light",
    underline = FALSE,
    bg = "#333333"
  )
)
