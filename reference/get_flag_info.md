# List Information About the Available Flags

Return a table that for all available flags returns the country name,
the continent, the flag code and the capital. The flag code must be used
as input in functions like
[`flag_icon()`](https://stibu81.github.io/simpleTaxonomy/reference/flag_icon.md)
and
[`run_taxonomy()`](https://stibu81.github.io/simpleTaxonomy/reference/run_taxonomy.md).

## Usage

``` r
get_flag_info(filter = NULL)
```

## Arguments

- filter:

  a regex pattern that is used to search in the country names. Search is
  case-insensitive.

## Value

a tibble with character columns `name`, `continent`, `code`, and
`capital`.
