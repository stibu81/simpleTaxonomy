# Enrich a Taxonomy File with URLs to Wikipedia-Images

Add URLs to Images from Wikipedia to a file with a taxonomic hierarchy.
By default, existing URLs are kept and only missing URLs are filled in.

## Usage

``` r
enrich_taxonomy_with_images(
  file,
  delim = ",",
  retry = FALSE,
  progress = TRUE,
  quiet = FALSE
)
```

## Arguments

- file:

  path to the csv file

- delim:

  the delimiter used in the file

- retry:

  Whether to retry images that have not been found previously.

- progress:

  Whether to show a progress bar.

- quiet:

  Should all output be suppressed?

## Value

a `taxonomy_graph` with added image URLs. The file given by `file` is
overwritten as a side effect.
