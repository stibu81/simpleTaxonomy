# Count the Number of Taxa per Rank

Count the number of taxa per rank in a `taxonomy_graph` and return the
result as a tibble.

## Usage

``` r
count_ranks(graph, subgraph = NULL, by_rank = NULL, only_major_ranks = FALSE)
```

## Arguments

- graph:

  a `taxonomy_graph` object

- subgraph:

  the name of a taxon which is to be used as the root of a subgraph, to
  which counting is restricted.

- by_rank:

  the name of a rank. Ranks will be counted for each of the taxa with
  this rank.

- only_major_ranks:

  should counts only be shown for the major ranks (Lebewesen, Domäne,
  Reich, Stamm, Klasse, Ordnung, Familie, Gattung, Art)?

## Value

a tibble

## Details

If `by_rank` is not used, the number of taxa is counted in the complete
graph or in the subgraph specified by `subgraph`.

`by_rank` can be used to count ranks grouped by the taxa of a given
rank. In this case, each row will correspond to a taxon of the requested
rank and each column will correspond to a rank.

## Examples

``` r
file <- get_example_taxonomy_file()
taxonomy <- read_taxonomy(file)

# count the complete graph
count_ranks(taxonomy)
#> # A tibble: 10 × 2
#>    rank             n
#>    <ord>        <int>
#>  1 Ordnung          1
#>  2 Unterordnung     2
#>  3 Überfamilie      1
#>  4 Familie         14
#>  5 Unterfamilie     2
#>  6 Tribus           2
#>  7 Gattung         13
#>  8 Art             57
#>  9 Unterart         3
#> 10 ohne Rang        1

# count only the cats
count_ranks(taxonomy, subgraph = "Katzen")
#> # A tibble: 4 × 2
#>   rank             n
#>   <ord>        <int>
#> 1 Familie          1
#> 2 Unterfamilie     2
#> 3 Gattung          3
#> 4 Art             12

# count caniformia by family
count_ranks(taxonomy, "Hundeartige", by_rank = "Familie")
#> # A tibble: 9 × 5
#>   Familie     Tribus Gattung   Art Unterart
#>   <chr>        <int>   <int> <int>    <int>
#> 1 Hunde            2       2     8        0
#> 2 Bären            0       1     8        3
#> 3 Walrosse         0       0     1        0
#> 4 Hundsrobben      0       0     4        0
#> 5 Ohrenrobben      0       1     3        0
#> 6 Marder           0       3    10        0
#> 7 Skunks           0       0     1        0
#> 8 Ailuridae        0       1     0        0
#> 9 Kleinbären       0       1     1        0
```
