library(shiny)
library(collapsibleTree)
library(simpleTaxonomy)

fluidPage(

  titlePanel("Taxonomy"),

  sidebarLayout(
    sidebarPanel(
      width = 2,
      # choices will be filled in the server in order to use server side
      # processing
      selectizeInput(
        "taxa_show",
        "Show taxa:",
        choices = NULL,
        multiple = TRUE
      ),
      checkboxInput(
        "full_expand",
        label = "Full expand",
        value = FALSE
      ),
      checkboxInput(
        "show_images",
        label = "Show images",
        value = TRUE
      ),
      # choices are filled here to avoid repeated rendering of the plot when
      # the app starts
      selectizeInput(
        "expand_ranks",
        label = "Always expand ranks:",
        choices = ranks,
        selected = expand_ranks_default,
        multiple = TRUE
      )
    ),

    mainPanel(
      width = 10,
      collapsibleTreeOutput("taxonomy_plot", height = "90vh")
    )
  )
)
