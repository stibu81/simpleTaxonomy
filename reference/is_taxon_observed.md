# Check if a Taxon Has Been Observed By a User

Check if a Taxon Has Been Observed By a User

## Usage

``` r
is_taxon_observed(taxon, user, research_grade_only = TRUE)
```

## Arguments

- taxon:

  character giving the scientific name of the taxon.

- user:

  character giving the user name.

- research_grade_only:

  logical indicating whether only research grade observations should be
  considered.

## Value

a logical value indicating whether the taxon has been observed by the
given user or not. If the function encounters any issues, it returns
`NA`.
