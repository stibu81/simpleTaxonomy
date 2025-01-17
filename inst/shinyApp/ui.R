library(shiny)
library(bslib)
library(collapsibleTree)
library(simpleTaxonomy)

page_sidebar(

  title = h4("simpleTaxonomy",
             img(src = "simpleTaxonomy_logo.png",
                 style = "position:absolute; right:20px; top:5px",
                 height = 42)),

  theme = bs_theme(preset = "yeti"),

  # Add custom CSS for rounded button with some space above
  tags$style(HTML("
    .btn-rounded {
      border-radius: 4px;
      margin-top: 10px;
    }
  ")),
  # Add custom CSS for background colour of accordeon panel and
  # collapsed accordeon header: same colour as sidebar background
  tags$style(HTML("
    .accordion-body {
      background-color: #f2f2f2;
    }
    .accordion-button.collapsed {
      background-color: #f2f2f2;
    }
  ")),


  sidebar = sidebar(
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
    ),
  ),

  card(
    collapsibleTreeOutput("taxonomy_plot")
  )
)
