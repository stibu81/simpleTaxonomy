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
#> # A tibble: 96 × 5
#>    parent       name         scientific               rank         image_url    
#>    <chr>        <chr>        <chr>                    <chr>        <chr>        
#>  1 NA           Raubtiere    Carnivora                Ordnung      https://uplo…
#>  2 Raubtiere    Hundeartige  Caniformia               Unterordnung https://uplo…
#>  3 Hundeartige  Hunde        Canidae                  Familie      https://uplo…
#>  4 Hunde        Echte Füchse Vulpini                  Tribus       https://uplo…
#>  5 Echte Füchse Vulpes       Vulpes                   Gattung      https://uplo…
#>  6 Vulpes       Polarfuchs   Vulpes lagopus           Art          https://uplo…
#>  7 Vulpes       Rotfuchs     Vulpes vulpes            Art          https://uplo…
#>  8 Vulpes       Wüstenfuchs  Vulpes zerda             Art          https://uplo…
#>  9 Echte Füchse Marderhund   Nyctereutes procyonoides Art          https://uplo…
#> 10 Hunde        Echte Hunde  Canini                   Tribus       https://uplo…
#> # ℹ 86 more rows
```
