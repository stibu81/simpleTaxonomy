# Enrich a Taxonomy File with iNaturalist Metadata

Fill missing values in the optional logical columns `local` and
`observed` using observation data from iNaturalist. Existing values are
retained.

## Usage

``` r
enrich_local_observed(
  file,
  delim = ",",
  local_country = NULL,
  user = NULL,
  progress = TRUE,
  quiet = FALSE
)
```

## Arguments

- file:

  path to the csv file

- delim:

  the delimiter used in the file

- local_country:

  character giving the name of the country that is considered local. If
  supplied, the taxonomy file must contain a `local` column.

- user:

  character giving the iNaturalist user name whose observations should
  be considered. If supplied, the taxonomy file must contain an
  `observed` column.

- progress:

  Whether to show a progress bar.

- quiet:

  Whether to suppress informational output while values are fetched.

## Value

a `taxonomy_graph` with added values in `local`, `observed`, or both.
The file given by `file` is overwritten as a side effect.

## Details

At least one of `local_country` and `user` must be supplied. If both are
supplied, both metadata columns are enriched.

Only leaf taxa with rank `Gattung`, `Art`, or `Unterart` are queried. A
taxon is considered local when iNaturalist contains at least two
research-grade observations from `local_country`. A taxon is considered
observed when iNaturalist contains at least one research-grade
observation by `user`.

To limit the number of API requests, at most 200 taxa are processed per
call.
