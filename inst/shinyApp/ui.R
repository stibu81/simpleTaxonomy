library(shiny)
library(bslib)
library(DT)
library(collapsibleTree)
library(simpleTaxonomy)

sidebar_tree <- sidebar(
  gap = "15px",
  padding = c("20px", "15px"),
  bg = "#F2F2F2",
  # choices will be filled in the server in order to use server side
  # processing
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
    choices = ranks,
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
      sliderInput(
        "image_size",
        label = "Bildgrösse:",
        min = 100, max = 400,
        step = 50,
        value = image_size_default
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
  gap = "15px",
  padding = c("20px", "15px"),
  bg = "#F2F2F2",
  # choices will be filled in the server in order to use server side
  # processing
  selectizeInput(
    "counts_root",
    "Zähle Taxa in:",
    choices = NULL,
    multiple = FALSE
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
  )
)

page_navbar(

  title = h4("simpleTaxonomy",
             img(src = "simpleTaxonomy_logo.png",
                 style = "position:absolute; right:20px; top:5px",
                 height = 42)),

  theme = bs_theme(preset = "yeti"),

  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "simpleTaxonomy.css"),
    tags$link(rel="icon", href="favicon-16x16.png"),
  ),

  nav_panel(
    "Baum",
    layout_sidebar(
      sidebar = sidebar_tree,
      card(collapsibleTreeOutput("taxonomy_plot"))
    )
  ),

  nav_panel(
    "Tabelle",
    layout_sidebar(
      sidebar = sidebar_counts,
      card(dataTableOutput("rank_counts"))
    )
  ),

  navbar_options = navbar_options(
    theme = "light",
    underline = FALSE,
    bg = "#333333"
  )
)
