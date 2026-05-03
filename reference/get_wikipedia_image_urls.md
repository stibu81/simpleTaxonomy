# Download URL for Wikipedia Images

Wikipedia pages usually have a thumbnail associated with them. This
function uses an API offered by Wikipedia to extract the URL of that
image.

## Usage

``` r
get_wikipedia_image_urls(
  taxa,
  contact = NULL,
  size = 120,
  lang = "de",
  progress = TRUE
)
```

## Arguments

- taxa:

  character with the taxa for which an image URL should be obtained.

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

- size:

  integer giving the width of the image in pixel.

- lang:

  language of the Wikipedia page to use. Available languages include
  "de" (German, the default), "en" (English), "fr" (French), and "es"
  (Spanish). See [List of
  Wikipedias](https://meta.wikimedia.org/wiki/List_of_Wikipedias#All_Wikipedias_ordered_by_number_of_articles)
  for a full list of available languages.

- progress:

  Whether to show a progress bar.

## Value

a character vector with URLs. For taxa where no thumbnail was found, the
function returns `NA`.
