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

# create a look-up table of taxa. This is needed to allow searching also for
# scientific names, because the common names must be passed to plot_taxonomy().
# only add the scientific name, if it is defined and not already contained in
# the common names.
use_sci <- !is.na(vertices$scientific) &
  !vertices$scientific %in% vertices$name
taxa <- c(vertices$name, vertices$name[use_sci])
names(taxa) <- c(vertices$name, vertices$scientific[use_sci])

# warn if there are duplicates
if (anyDuplicated(names(taxa)) > 0) {
  warning("There are duplicate taxa.")
}
