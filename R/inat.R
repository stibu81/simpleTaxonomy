base_url <- "https://api.inaturalist.org/v2/"

#' Check if a Taxon is Local to a Country
#' 
#' @param taxon character giving the scientific name of the taxon.
#' @param country character giving the name of the country that is considered
#'  to be local. 
#' @param n_min the minimal number of observations that must be found for the
#'  taxon to be accepted as local.
#' 
#' @details
#' Only research grade observations are taken into account to reduce the chance
#' that misidentified observations skew the result.
#' 
#' @returns
#' a logical value indicating whether the taxon is local to the given country
#' or not. If the function encounters any issues, it returns `NA`.
#' 
#' @export

is_taxon_local <- function(
  taxon, 
  country,
  n_min = 2
) {
  taxon <- stringr::str_squish(taxon)
  taxon_id <- get_taxon_id(taxon)
  if (is.na(taxon_id)) return(NA)
  
  country_id <- get_country_id(country)
  if (is.na(country_id)) return(NA)

  obs_count <- count_observations(taxon_id, country_id = country_id)

  # more observations than the threshold?
  obs_count >= n_min
}


#' Check if a Taxon Has Been Observed By a User
#' 
#' @param taxon character giving the scientific name of the taxon.
#' @param user character giving the user name.
#' @param research_grade_only logical indicating whether only research
#'  grade observations should be considered.
#' 
#' @returns
#' a logical value indicating whether the taxon has been observed by the given
#' user or not. If the function encounters any issues, it returns `NA`.
#' 
#' @export

is_taxon_observed <- function(taxon, user, research_grade_only = TRUE) {
  taxon <- stringr::str_squish(taxon)
  taxon_id <- get_taxon_id(taxon)
  if (is.na(taxon_id)) return(NA)

  obs_count <- count_observations(
    taxon_id, 
    user = user, 
    research_grade_only = research_grade_only
  )

  obs_count >= 1
}


get_taxon_id <- function(taxon) {
  # often, the function get_taxon_id() will be called with the same taxon
  # multiple times time. => implement a very simple caching mechanism.
  cache <- getOption(
    "simpleTaxonomy_taxon_cache",
    default = list(taxon = "", id = NA_integer_)
  )
  if (taxon == cache$taxon) return(cache$id)

  url <- glue::glue(
    "{base_url}taxa?q={utils::URLencode(taxon)}&per_page=100&fields=id%2Cname"
  )
  data <- jsonlite::fromJSON(url)

  taxon_results <- dplyr::as_tibble(data$results)
  if (data$total_results > 0) {
    taxon_results <- taxon_results %>% dplyr::filter(.data$name == taxon)
  }
  n_taxa <- nrow(taxon_results)
  if (n_taxa == 0) {
    cli::cli_alert_danger("Taxon '{taxon}' not found on iNaturalist.")
    return(NA)
  }
  if (n_taxa > 1) {
    cli::cli_alert_danger("Taxon '{taxon}' is not unique on iNaturalist.")
    return(NA)
  }

  options(
    simpleTaxonomy_taxon_cache = list(taxon = taxon, id = taxon_results$id)
  )

  taxon_results$id
}


get_country_id <- function(country) {
  # usually, the function is_local_taxon() will be called with the same country
  # each time. => implement a very simple caching mechanism.
  cache <- getOption(
    "simpleTaxonomy_country_cache",
    default = list(country = "", id = NA_integer_)
  )
  if (country == cache$country) return(cache$id)

  url <- glue::glue(
    "{base_url}places?q={utils::URLencode(country)}&order_by=area&",
    "per_page=100&fields=id%2Cdisplay_name%2Cplace_type%2Carea"
  )
  data <- jsonlite::fromJSON(url)

  country_results <- dplyr::as_tibble(data$results)
  if (data$total_results > 0) {
      country_results <- country_results %>% 
        dplyr::filter(.data$place_type == 12)
  }
  n_countries <- nrow(country_results)
  if (n_countries == 0) {
    cli::cli_alert_danger("Country '{country}' not found on iNaturalist.")
    return(NA)
  }
  if (n_countries > 1) {
    cli::cli_alert_danger("Several countries found for '{country}' on iNaturalist.")
    return(NA)
  }

  options(
    simpleTaxonomy_country_cache = list(country = country, id = country_results$id)
  )
  
  country_results$id
}


count_observations <- function(
  taxon_id,
  country_id = NULL,
  user = NULL,
  research_grade_only = TRUE
) {

  url <- glue::glue(
    "{base_url}observations?captive=false&taxon_id={taxon_id}&per_page=1"
  )
  if (research_grade_only) {
    url <- glue::glue(url, "&quality_grade=research")
  }
  if (!is.null(country_id)) {
    url <- glue::glue(url, "&place_id={country_id}")
  }
  if (!is.null(user)) {
    url <- glue::glue(url, "&user_login={user}")
  }
  data <- jsonlite::fromJSON(url)

  data$total_results
}