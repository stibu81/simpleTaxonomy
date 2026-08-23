# Read Taxonomic Hierarchy From File

Read a csv file that contains the data that defines a taxonomic
hierarchy and return it as a graph.

## Usage

``` r
read_taxonomy(file, delim = ",")
```

## Arguments

- file:

  path to the csv file

- delim:

  the delimiter used in the file

## Value

a `taxonomy_graph` object which inherits from `igraph`

## Details

The file must contain column names in the first row. Each row defines a
taxon with the following attributes:

- parent:

  The name of the parent taxon. This column must be empty for the root
  taxon.

- name:

  The name of the taxon that should be used to label the nodes, e.g.,
  the common name. These names must be unique.

- scientific:

  The scientific name of the taxon.

- rank:

  The rank of the taxon, e.g., "Familie", "Art", or similar (only German
  ranks are supported so far). These will be used to colour the nodes.

The function checks that the file satisfies the following conditions:

- The required columns are all present.

- There is exactly one root taxon, which is a taxon without parent.

- There are no duplicated names.

- Each parent taxon is defined by its own row in the table.

- The rank is always defined.

In addition, the function also warns if an unknown rank is used. This
will still lead to a graph that can be plotted, but the nodes with
unknown rank will not be coloured.

The common names (column "name") of the taxa must be unique, because
they are used to create the graph. Sometimes, there are taxa that have
identical common names, e.g. the family Equidae and the genus Equus are
both called "Pferde" in German. In theses cases, one can use an
additional identifier in parenthesis to make the names unique, e.g.,
"Pferde (F)" and "Pferde" for the family and the genus, respectively.
The identifier "(F)" will be removed and not be shown in the
visualisation.

## Examples

``` r
file <- get_example_taxonomy_file()
taxonomy <- read_taxonomy(file)
class(taxonomy)
#> [1] "taxonomy_graph" "igraph"        
as_tibble(taxonomy)
#> # A tibble: 98 × 8
#>    parent       name         scientific   rank  extinct local observed image_url
#>    <chr>        <chr>        <chr>        <chr> <lgl>   <lgl> <lgl>    <chr>    
#>  1 NA           Raubtiere    Carnivora    Ordn… NA      NA    NA       https://…
#>  2 Raubtiere    Hundeartige  Caniformia   Unte… NA      NA    NA       https://…
#>  3 Hundeartige  Hunde        Canidae      Fami… NA      NA    NA       https://…
#>  4 Hunde        Echte Füchse Vulpini      Trib… NA      NA    NA       https://…
#>  5 Echte Füchse Vulpes       Vulpes       Gatt… NA      NA    NA       https://…
#>  6 Vulpes       Polarfuchs   Vulpes lago… Art   NA      NA    NA       https://…
#>  7 Vulpes       Rotfuchs     Vulpes vulp… Art   NA      TRUE  TRUE     https://…
#>  8 Vulpes       Wüstenfuchs  Vulpes zerda Art   NA      NA    NA       https://…
#>  9 Echte Füchse Marderhund   Nyctereutes… Art   NA      TRUE  NA       https://…
#> 10 Hunde        Echte Hunde  Canini       Trib… NA      NA    NA       https://…
#> # ℹ 88 more rows
```
