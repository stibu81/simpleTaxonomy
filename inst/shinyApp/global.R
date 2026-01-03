library(simpleTaxonomy)
library(igraph)
library(dplyr)

# define settings: take from options, if defined, use defaults otherwise.
# the options are used to pass settings from run_taxonomy() to the app.
taxonomy_file <- getOption(
  "simpleTaxonomy_file",
  paste0("https://raw.githubusercontent.com/",
         "stibu81/taxonomyData/refs/heads/main/taxonomy.csv")
)
expand_ranks_default <- getOption("simpleTaxonomy_expand_ranks",
                                  c("Gattung", "Art", "Unterart"))
image_size_default <- getOption("simpleTaxonomy_image_size", 150)
link_length_default <- getOption("simpleTaxonomy_link_length", 200)


# read the taxonomy
taxonomy <- read_taxonomy(taxonomy_file)
# only get the nodes here, which is much faster than as_tibble(taxonomy)
vertices <- as_tibble(vertex_attr(taxonomy))


# create a vector of ranks that appear in the data. In order to have them
# sorted correctly, take them from available_ranks()
ranks <- available_ranks() %>%
  filter(.data$de %in% vertices$rank) %>%
  # don't list the first rank and ranks that have no defined level in the
  # hierarchy (e.g., "ohne Rang")
  filter(level > 1) %>%
  pull("de")

# we need the common and scientific names of taxa that are not leaves,
# i.e. that have children
no_leaf_taxa <- attr(taxonomy, "match_labs")[
  !attr(taxonomy, "match_labs") %in% names(get_leaf_nodes(taxonomy))
] %>% names()
