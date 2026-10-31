# Check if a Taxon is Local to a Country

Check if a Taxon is Local to a Country

## Usage

``` r
is_taxon_local(taxon, country, n_min = 2)
```

## Arguments

- taxon:

  character giving the scientific name of the taxon.

- country:

  character giving the name of the country that is considered to be
  local.

- n_min:

  the minimal number of observations that must be found for the taxon to
  be accepted as local.

## Value

a logical value indicating whether the taxon is local to the given
country or not. If the function encounters any issues, it returns `NA`.

## Details

Only research grade observations are taken into account to reduce the
chance that misidentified observations skew the result.
