# Find Taxa By Pattern

Find taxa by pattern and return their common names. The output can be
passed to the arguments `show`, `full_expand` or `focus` in
[`plot_taxonomy()`](https://stibu81.github.io/simpleTaxonomy/reference/plot_taxonomy.md).

## Usage

``` r
find_taxon(graph, pattern, target = c("all", "name", "scientific"))
```

## Arguments

- graph:

  a `taxonomy_graph` object, typically created with
  [`read_taxonomy()`](https://stibu81.github.io/simpleTaxonomy/reference/read_taxonomy.md).

- pattern:

  a regex pattern, matching is case insensitive.

- target:

  character giving the column to search in ("name" or "scientific"). The
  default is "all" which searches in all columns.

## Value

a character vector giving the taxon names that match the pattern

## Examples

``` r
file <- get_example_taxonomy_file()
taxonomy <- read_taxonomy(file)
find_taxon(taxonomy, "katze")
#>  [1] "Katzenartige"            "Katzen"                 
#>  [3] "Kleinkatzen"             "Echte Katzen"           
#>  [5] "Europäische Wildkatze"   "Hauskatze"              
#>  [7] "Pardelkatzen"            "Grosskatzen"            
#>  [9] "Eigentliche Grosskatzen" "Schleichkatzen"         
#> [11] "Afrikanische Zibetkatze" "Ginsterkatzen"          
find_taxon(taxonomy, "felis")
#> [1] "Echte Katzen"          "Europäische Wildkatze" "Hauskatze"            
#> [4] "Nebelparder"          
```
