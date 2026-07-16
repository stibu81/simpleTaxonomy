# print.taxonomy_graph() works with default values

    Code
      print(taxonomy)
    Output
      taxonomy_graph with 96 nodes.
      root node: Raubtiere 
      tree depth: 6 
      number of leaves: 63 
      graph has image url for 95 nodes.
      
      most common ranks:
       rank         n 
       Art          57
       Familie      14
       Gattung      13
       Unterart      3
       Unterordnung  2
       Unterfamilie  2
       Tribus        2

# print.taxonomy_graph() works with less ranks

    Code
      print(taxonomy, n_ranks = 3)
    Output
      taxonomy_graph with 96 nodes.
      root node: Raubtiere 
      tree depth: 6 
      number of leaves: 63 
      graph has image url for 95 nodes.
      
      most common ranks:
       rank    n 
       Art     57
       Familie 14
       Gattung 13

# print.taxonomy_graph() works without rank summary

    Code
      print(taxonomy, n_ranks = 0)
    Output
      taxonomy_graph with 96 nodes.
      root node: Raubtiere 
      tree depth: 6 
      number of leaves: 63 
      graph has image url for 95 nodes.

# print.taxonomy_graph() works if there are no image urls

    Code
      print(taxonomy_no_images)
    Output
      taxonomy_graph with 96 nodes.
      root node: Raubtiere 
      tree depth: 6 
      number of leaves: 63 
      graph has no image urls.
      
      most common ranks:
       rank         n 
       Art          57
       Familie      14
       Gattung      13
       Unterart      3
       Unterordnung  2
       Unterfamilie  2
       Tribus        2

