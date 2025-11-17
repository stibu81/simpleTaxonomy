# Download URL for Wikipedia Images

Wikipedia pages usually have a thumbnail associated with them. This
function uses an API offered by Wikipedia to extract the URL of that
image.

## Usage

``` r
get_wikipedia_image_urls(taxa, size = 100, lang = "de", progress = TRUE)
```

## Arguments

- taxa:

  character with the taxa for which an image URL should be obtained.

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
