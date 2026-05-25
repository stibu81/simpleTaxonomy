library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(simpleTaxonomy)

# define settings: take from options, if defined, use defaults otherwise.
# the options are used to pass settings from run_taxonomy() to the app.
taxonomy_file <- getOption(
  "simpleTaxonomy_file",
  paste0("https://raw.githubusercontent.com/",
         "stibu81/taxonomyData/refs/heads/main/taxonomy.csv")
)
root <- getOption("simpleTaxonomy_root", "")
expand_ranks_default <- getOption("simpleTaxonomy_expand_ranks",
                                  c("Gattung", "Art", "Unterart"))
image_size_default <- getOption("simpleTaxonomy_image_size", "250")
link_length_default <- getOption("simpleTaxonomy_link_length", 200)

logger::log_info("start simpleTaxonomy shiny app with the following settings:")
logger::log_info("taxonomy_file: {taxonomy_file}")
logger::log_info("root: '{root}'")
logger::log_info("expand_ranks: '{paste(expand_ranks_default, collapse = '\\', \\'')}'")
logger::log_info("image_size: {image_size_default}")
logger::log_info("link_length: {link_length_default}")

