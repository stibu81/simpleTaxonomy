# print.taxonomy_graph() works with default values

    Code
      print(taxonomy)
    Output
      taxonomy_graph with 98 nodes.
      root node: Raubtiere 
      tree depth: 6 
      number of leaves: 64 
      graph has image url for 97 nodes.
      number of observed taxa [(sub-)species]: 2 [2] 
      number of local taxa [(sub-)species]: 15 [15] 
      
      most common ranks:
       rank         n 
       Art          57
       Familie      14
       Gattung      14
       Unterfamilie  3
       Unterart      3

# print.taxonomy_graph() works with less ranks

    Code
      print(taxonomy, n_ranks = 3)
    Output
      taxonomy_graph with 98 nodes.
      root node: Raubtiere 
      tree depth: 6 
      number of leaves: 64 
      graph has image url for 97 nodes.
      number of observed taxa [(sub-)species]: 2 [2] 
      number of local taxa [(sub-)species]: 15 [15] 
      
      most common ranks:
       rank    n 
       Art     57
       Familie 14
       Gattung 14

# print.taxonomy_graph() works without rank summary

    Code
      print(taxonomy, n_ranks = 0)
    Output
      taxonomy_graph with 98 nodes.
      root node: Raubtiere 
      tree depth: 6 
      number of leaves: 64 
      graph has image url for 97 nodes.
      number of observed taxa [(sub-)species]: 2 [2] 
      number of local taxa [(sub-)species]: 15 [15] 

# print.taxonomy_graph() works if there are no image urls

    Code
      print(taxonomy_no_images)
    Output
      taxonomy_graph with 98 nodes.
      root node: Raubtiere 
      tree depth: 6 
      number of leaves: 64 
      graph has no image urls.
      number of observed taxa [(sub-)species]: 2 [2] 
      number of local taxa [(sub-)species]: 15 [15] 
      
      most common ranks:
       rank         n 
       Art          57
       Familie      14
       Gattung      14
       Unterfamilie  3
       Unterart      3

# print.taxonomy_graph() works if observed and local are absent

    Code
      print(taxonomy_no_meta)
    Output
      taxonomy_graph with 98 nodes.
      root node: Raubtiere 
      tree depth: 6 
      number of leaves: 64 
      graph has image url for 97 nodes.
      
      most common ranks:
       rank         n 
       Art          57
       Familie      14
       Gattung      14
       Unterfamilie  3
       Unterart      3

