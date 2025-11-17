# Get Depth of a Taxonomy Tree Graph

Get the depth of a taxonomy tree graph, i.e., the number of nodes from
the root to the most distant leaf.

## Usage

``` r
get_tree_depth(graph)
```

## Arguments

- graph:

  a `taxonomy_graph` object

## Value

integer giving the depth of the graph

## Examples

``` r
file <- get_example_taxonomy_file()
taxonomy <- read_taxonomy(file)
get_tree_depth(taxonomy)
#> [1] 6
```
