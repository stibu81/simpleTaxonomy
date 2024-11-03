library(shiny)
library(bslib)
library(collapsibleTree)
library(simpleTaxonomy)

page_sidebar(

  title = "simpleTaxonomy",

  theme = bs_theme(preset = "yeti"),

  sidebar = sidebar(
      # choices will be filled in the server in order to use server side
      # processing
      selectizeInput(
        "taxa_show",
        "Show taxa:",
        choices = NULL,
        multiple = TRUE
      ),
      input_switch(
        "full_expand",
        label = "Full expand",
        value = FALSE
      ),
      input_switch(
        "highlight",
        label = "Highlight",
        value = FALSE
      ),
      # choices are filled here to avoid repeated rendering of the plot when
      # the app starts
      selectizeInput(
        "expand_ranks",
        label = "Always expand ranks:",
        choices = ranks,
        selected = expand_ranks_default,
        multiple = TRUE
      ),
      input_switch(
        "show_images",
        label = "Show images",
        value = TRUE
      ),
      sliderInput(
        "image_size",
        label = "Image size:",
        min = 100, max = 400,
        step = 50,
        value = image_size_default
      )
    ),

    card(
      collapsibleTreeOutput("taxonomy_plot")
    )
)
