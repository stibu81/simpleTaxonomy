library(simpleTaxonomy)
library(igraph)
library(dplyr)

# define settings: take from options, if defined, use defaults otherwise.
# the options are used to pass settings from run_taxonomy() to the app.
taxonomy_file <- simpleTaxonomy:::get_option_or_default(
  "simpleTaxonomy_file",
  paste0("https://raw.githubusercontent.com/",
         "stibu81/taxonomyData/refs/heads/main/taxonomy.csv")
)
expand_ranks_default <- simpleTaxonomy:::get_option_or_default(
  "simpleTaxonomy_expand_ranks",
  c("Gattung", "Art", "Unterart")
)
image_size_default <- simpleTaxonomy:::get_option_or_default(
  "simpleTaxonomy_image_size",
  150
)


# read the taxonomy
taxonomy <- read_taxonomy(taxonomy_file)
taxonomy_df <- as_tibble(taxonomy)


# create a vector of ranks that appear in the data. In order to have them
# sorted correctly, take them from get_rank_colours()
ranks <- simpleTaxonomy:::get_rank_colours() %>%
  filter(.data$rank %in% taxonomy_df$rank) %>%
  pull("rank") %>%
  # don't list the rank of the root node
  setdiff(vertex_attr(taxonomy, "rank", get_root_node(taxonomy)))

# create a look up table of taxa. This is needed to allow searching also for
# scientific names, because the common names must be passed to plot_taxonomy().
# only add the scientific name, if it is defined and not already contained in
# the common names.
use_sci <- !is.na(taxonomy_df$scientific) &
  !taxonomy_df$scientific %in% taxonomy_df$name
taxa <- c(taxonomy_df$name, taxonomy_df$name[use_sci])
names(taxa) <- c(taxonomy_df$name, taxonomy_df$scientific[use_sci])

# warn if there are duplicates
if (anyDuplicated(names(taxa)) > 0) {
  warning("There are duplicate taxa.")
}
