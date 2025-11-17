# Extract a Subgraph from a Taxonomy Graph

Extract a subgraph from a `taxonomy_graph` below a given taxon.

## Usage

``` r
get_subgraph(graph, taxon)
```

## Arguments

- graph:

  a `taxonomy_graph` object

- taxon:

  character giving a single taxon. The subgraph will contain this taxon
  as the root and all the taxa that are in the tree below it.

## Value

a `taxonomy_graph` object

## Examples

``` r
file <- get_example_taxonomy_file()
taxonomy <- read_taxonomy(file)
cats <- get_subgraph(taxonomy, "Katzen")
```
