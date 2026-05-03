# Enrich a Taxonomy File with URLs to Wikipedia-Images

Add URLs to Images from Wikipedia to a file with a taxonomic hierarchy.
By default, existing URLs are kept and only missing URLs are filled in.

## Usage

``` r
enrich_taxonomy_with_images(
  file,
  delim = ",",
  contact = NULL,
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

- contact:

  contact data to pass on in the user agent of the API call to
  Wikipedia. Starting in 2026, there is a strict rate limit on API calls
  per minute for calls without contact data. There are two possible ways
  to pass contact data:

  - a string containing an email address.

  - a list that specifies an existing Wikipedia account. You need to
    specify two named elements: "lang" giving the language code of the
    Wikipedia where the account is registered and "user" giving the user
    name. Example: `list(lang = "de", user = "James Bond")`.

  See the [User Agent
  Policy](https://foundation.wikimedia.org/wiki/Policy:Wikimedia_Foundation_User-Agent_Policy)
  for more information.

- retry:

  Whether to retry images that have not been found previously.

- progress:

  Whether to show a progress bar.

- quiet:

  Should all output be suppressed?

## Value

a `taxonomy_graph` with added image URLs. The file given by `file` is
overwritten as a side effect.
