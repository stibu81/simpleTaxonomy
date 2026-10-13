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
  rlang::check_installed("inatpick")

  taxon <- stringr::str_squish(taxon)
  taxon_id <- get_taxon_id(taxon)
  if (is.na(taxon_id)) return(NA)
  
  country_id <- get_country_id(country)
  if (is.na(country_id)) return(NA)

  observations <- get_observations(taxon_id, country_id = country_id, n_min = n_min)

  # more observations than the threshold?
  nrow(observations) >= n_min
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
  rlang::check_installed("inatpick")

  taxon <- stringr::str_squish(taxon)
  taxon_id <- get_taxon_id(taxon)
  if (is.na(taxon_id)) return(NA)

  observations <- get_observations(
    taxon_id, 
    user = user, 
    research_grade_only = research_grade_only
  )

  nrow(observations) >= 1
}


get_taxon_id <- function(taxon) {
  taxon_results <- suppressMessages(
    inatpick::inat_search_taxon(taxon, n = 100)
  )
  # filter for results that match the exact name. This can only be done after
  # checking for the existence of the column "name" because inat_search_taxon()
  # returns a data frame without columns if nothing is found.
  if ("name" %in% names(taxon_results)) {
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

  country_results <- suppressMessages(
    inatpick::inat_search_place(country, n = 100)
  )
  # filter for countries (place_type = 12). This can only be done after
  # checking for the existence of the column "place_type" because
  # inat_search_place() returns a data frame without columns if nothing is
  # found.
  if ("place_type" %in% names(country_results)) {
    country_results <- country_results %>% dplyr::filter(.data$place_type == 12)
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


get_observations <- function(
  taxon_id,
  country_id = NULL,
  user = NULL,
  research_grade_only = TRUE,
  n_min = 1
) {
  quality_grade = if (research_grade_only) "research" else "any"
  # because downloading the observations can be very slow for common taxa, 
  # we start by just downloading data for one year. Only if there is not 
  # enough data, a full download is performed.
  observations <- suppressMessages(
    inatpick::inat_fetch(taxon_id, country_id,
                         year = as.integer(format(Sys.Date(), "%Y")) - 1,
                         user_login = user,
                         quality_grade = quality_grade, verbose = FALSE)
  )
  if (nrow(observations) < n_min) {
    observations <- suppressMessages(
      inatpick::inat_fetch(taxon_id, country_id, user_login = user,
                           quality_grade = quality_grade, verbose = FALSE)
    )
  }

  observations
}