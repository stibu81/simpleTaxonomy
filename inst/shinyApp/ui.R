library(shiny)
library(bslib)
library(collapsibleTree)
library(simpleTaxonomy)

page_sidebar(

  title = "simpleTaxonomy",

  theme = bs_theme(preset = "yeti"),

  # Add custom CSS for rounded button with some space above
  tags$style(HTML("
    .btn-rounded {
      border-radius: 4px;
      margin-top: 10px;
    }
  ")),

  sidebar = sidebar(
    gap = "20px",
    padding = c("20px", "15px"),
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
    ),
    uiOutput("wikipedia_link")
  ),

  card(
    collapsibleTreeOutput("taxonomy_plot")
  )
)
