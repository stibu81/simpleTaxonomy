library(shiny)
library(bslib)
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
image_size_default <- getOption("simpleTaxonomy_image_size", 150)
link_length_default <- getOption("simpleTaxonomy_link_length", 200)

logger::log_info("start simpleTaxonomy shiny app with the following settings:")
logger::log_info("taxonomy_file: {taxonomy_file}")
logger::log_info("root: '{root}'")
logger::log_info("expand_ranks: '{paste(expand_ranks_default, collapse = '\\', \\'')}'")
logger::log_info("image_size: {image_size_default}")
logger::log_info("link_length: {link_length_default}")

# read the taxonomy
logger::log_info("reading the file and preparing data ...")
taxonomy <- read_taxonomy(taxonomy_file)
# only get the nodes here, which is much faster than as_tibble(taxonomy)
vertices <- as_tibble(igraph::vertex_attr(taxonomy))

# determine the initial root taxon
initial_root <- get_taxon_names(taxonomy, getOption("simpleTaxonomy_root"))
if (length(initial_root) != 1 || is.na(initial_root)) {
  initial_root <- names(get_root_node(taxonomy))
}
logger::log_info("selected root: '{initial_root}'")

# create a vector of ranks that appear in the data. In order to have them
# sorted correctly, take them from available_ranks()
ranks <- available_ranks() %>%
  dplyr::filter(.data$de %in% vertices$rank) %>%
  # don't list the first rank and ranks that have no defined level in the
  # hierarchy (e.g., "ohne Rang")
  dplyr::filter(.data$level > 1) %>%
  dplyr::pull("de")

# we need the common and scientific names of taxa that are not leaves,
# i.e. that have children
no_leaf_taxa <- attr(taxonomy, "match_labs")[
  !attr(taxonomy, "match_labs") %in% names(get_leaf_nodes(taxonomy))
] %>% names()

logger::log_info("preparation completed")
