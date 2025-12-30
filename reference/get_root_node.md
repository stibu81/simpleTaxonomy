# Get the Root Node and the Leaves of a Taxonomy Graph

Get the Root Node and the Leaves of a Taxonomy Graph

## Usage

``` r
get_root_node(graph)

get_leaf_nodes(graph)
```

## Arguments

- graph:

  a `taxonomy_graph` object

## Value

a `igraph.vs` object corresponding to the root node (for
`get_root_node()`) or the leaf nodes (for `get_leaf_nodes()`) of the
graph.

## Examples

``` r
file <- get_example_taxonomy_file()
taxonomy <- read_taxonomy(file)
get_root_node(taxonomy)
#> + 1/96 vertex, named, from 7e36c9d:
#> [1] Raubtiere
get_leaf_nodes(taxonomy)
#> + 63/96 vertices, named, from 7e36c9d:
#>  [1] Polarfuchs                Rotfuchs                 
#>  [3] Wüstenfuchs               Marderhund               
#>  [5] Afrikanischer Wildhund    Wolf                     
#>  [7] Kojote                    Goldschakal              
#>  [9] Amerikanischer Schwarzbär Europäischer Braunbär    
#> [11] Grizzlybär                Kodiakbär                
#> [13] Eisbär                    Kragenbär                
#> [15] Grosser Panda             Brillenbär               
#> [17] Malaienbär                Lippenbär                
#> [19] Walross                   Südlicher See-Elefant    
#> + ... omitted several vertices
```
