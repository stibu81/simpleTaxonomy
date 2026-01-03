# Get Taxon Labels by Common or Scientific Name

The Taxa in the `taxonomy_graph` are labelled by their common name. This
function returns the label for a vector of common and/or scientific
names. It is much more efficient than
[`find_taxon()`](https://stibu81.github.io/simpleTaxonomy/reference/find_taxon.md)
because it relies on a matching table stored in the `taxonomy_graph`.

## Usage

``` r
get_taxon_names(graph, char)
```

## Arguments

- graph:

  a `taxonomy_graph` object, typically created with
  [`read_taxonomy()`](https://stibu81.github.io/simpleTaxonomy/reference/read_taxonomy.md).

- char:

  a character vector of common and/or scientific names to be converted
  to labels. Matching is case-sensitive.

## Value

a character vector of the same length as `char` containing taxon labels.

## See also

find_taxon
