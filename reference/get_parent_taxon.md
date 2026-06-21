# Get the Parent Taxon of a Taxon

Get the Parent Taxon of a Taxon

## Usage

``` r
get_parent_taxon(graph, taxon)
```

## Arguments

- graph:

  a `taxonomy_graph` object, typically created with
  [`read_taxonomy()`](https://stibu81.github.io/simpleTaxonomy/reference/read_taxonomy.md).

- taxon:

  character of length 1 giving the exact common or scientific name of a
  taxon.

## Value

a character vector of length 1 giving the label of the parent taxon of
`taxon`, except when `taxon` is the root, in which case an empty
character vector is returned.
