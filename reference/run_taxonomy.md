# Run simpleTaxonomy Shiny App

Run the simple shiny app that is included with simpleTaxonomy. The app
provides several features to interact with a `taxonomy_graph`. A
`taxonomy_graph` can be read from a file or an URL. By default, the app
loads the data from <https://github.com/stibu81/taxonomyData>.

## Usage

``` r
run_taxonomy(
  file = NULL,
  root = NULL,
  expand_ranks = c("Gattung", "Art", "Unterart"),
  image_size = 150,
  link_length = 200,
  launch_browser = NULL
)
```

## Arguments

- file:

  Path to a file containing a `taxonomy_graph`. If omitted, the data
  from <https://github.com/stibu81/taxonomyData> is used.

- root:

  character giving the common or scientific name of the taxon to select
  as root. If omitted, the actual root of the taxonomy graph is used. If
  the selected root does not exist in the graph, the app also falls back
  on the actual root of the graph.

- expand_ranks:

  a list of ranks that should always be expanded. This can be changed
  interactively in the app.

- image_size:

  numeric giving the default image width in pixels used in the app. The
  user can change this interactively in the app.

- link_length:

  numeric giving the default length of links in pixels used in the app.
  The user can change this interactively in the app.

- launch_browser:

  logical, if `TRUE`, the application is opened in the system's default
  browser, if `FALSE`, no browser is started. If the argument is
  omitted, the value according to the option `shiny.launch.browser` is
  used, which in RStudio opens the internal shiny viewer.
