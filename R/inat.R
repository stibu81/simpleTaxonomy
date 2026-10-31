base_url <- "https://api.inaturalist.org/v2/"

#' Fill in local and observed with iNaturalist
#' 
#' @export

enrich_local_observed <- function(file,
                                  delim = ",",
                                  local_country = NULL,
                                  user = NULL,
                                  progress = TRUE,
                                  quiet = FALSE) {
  
  if (is.null(local_country) && is.null(user)) {
    cli::cli_abort("You must specify at least one of `local_country` and `user`.")
  }
  
  taxonomy <- read_taxonomy(file, delim)

  # which columns must be checked
  meta_cols <- c()
  if (!is.null(local_country)) {
    cntry_id <- suppressWarnings(get_country_id(local_country))
    if (is.na(cntry_id)) {
      cli::cli_abort("Country '{local_country}' not found on iNaturalist.")
    }
    meta_cols <- c(meta_cols, "local")
  }
  if (!is.null(user)) {
    meta_cols <- c(meta_cols, "observed")
  }

  leaves <- get_leaf_nodes(taxonomy)
  leaf_attrs <- igraph::vertex_attr(taxonomy, index = leaves)
  
  # if any of the needed metadata columns is missing, abort
  missing_meta <- !(meta_cols %in% names(leaf_attrs))
  if (any(missing_meta)) {
    mc <- meta_cols[missing_meta]
    cli::cli_abort("Column{?s} {.str {mc}} {?is/are} missing in the taxonomy file.")
  }

  # extract those leaf nodes where at least one of the metadata columns is empty
  # only keep ranks genus, species, and subspecies 
  # we also need the scientific name to run the queries
  meta_attrs <- dplyr::as_tibble(leaf_attrs[c("scientific", meta_cols)])
  keep_ranks <- c("Gattung", "Art", "Unterart")
  i_meta_missing <- which(
    rowSums(is.na(meta_attrs)) > 0 & leaf_attrs$rank %in% keep_ranks
  )
  n_missing <- length(i_meta_missing)

  if (n_missing == 0) {
    cli::cli_alert_info("No missing meta data.")
    return(taxonomy)
  }
  
  # to avoid extremely long runtimes, limit the number of downloads
  n_max <- 200
  if (n_missing > n_max) {
    if (!quiet) {
      cli::cli_alert_info(
        c("there are {n_missing} taxa with missing metadata, ",
          "trying to get values for {n_max} of them."))
    }
    i_meta_missing <- i_meta_missing[1:n_max]
  } else if (!quiet) {
    cli::cli_alert_info(
      "trying to get values for {n_missing} tax{?on/a} with missing metadata."
    )
  }

  leaves_missing <- leaves[i_meta_missing]
  meta_attrs <- meta_attrs[i_meta_missing, ]

  filled_attrs <- fill_meta_attrs(
    meta_attrs,
    local_country = local_country,
    user = user,
    progress = progress
  )

  for (meta_col in meta_cols) {
    taxonomy <- taxonomy %>% 
      igraph::set_vertex_attr(meta_col, leaves_missing, filled_attrs[[meta_col]])
  }
  
  write_taxonomy_csv(taxonomy, file, delim)

  taxonomy
}


fill_meta_attrs <- function(
  meta_attrs, 
  local_country, 
  user, 
  progress, 
  error_call = rlang::caller_env()
) {
  filled_meta_attrs <- tryCatch(
    suppressWarnings(
      purrr::map(
        1:nrow(meta_attrs),
        \(i_row) fill_meta_data_one(meta_attrs[i_row, ],
                                    local_country = local_country,
                                    user = user),
        .progress = progress)
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "x" = "Connection to {base_url} failed.",
          "i" = "Message: {e$parent$message}",
          "i" = "Check your internet connection."
        ),
        call = error_call
      )
    }
  )

  dplyr::bind_rows(filled_meta_attrs)
}


fill_meta_data_one <- function(meta_attrs, local_country, user) {
  # add some waiting time to honour the rate limit of 60 requests per minute
  if ("local" %in% names(meta_attrs) && is.na(meta_attrs["local"])) {
    meta_attrs["local"] <- is_taxon_local(meta_attrs$scientific, local_country)
  }
  if ("observed" %in% names(meta_attrs) && is.na(meta_attrs["observed"])) {
    meta_attrs["observed"] <- is_taxon_observed(meta_attrs$scientific, user)
  }
  meta_attrs
}


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
  
  # the API seems to fail on search term longer than thirty characters => shorten
  taxon_short <- stringr::str_sub(taxon, end = 30)

  url <- glue::glue(
    "{base_url}taxa?q={utils::URLencode(taxon_short)}&per_page=200&",
    "fields=id%2Cname%2Crank"
  )
  data <- req_inat(url)

  taxon_results <- dplyr::as_tibble(data$results)
  if (data$total_results > 0) {
    taxon_results <- taxon_results %>% 
      dplyr::filter(.data$name == taxon,
                    # only genus, species, subspecies are relevant. The filter is 
                    # important because, e.g., a complex has the name of a species.
                    .data$rank %in% c("genus", "species", "subspecies"))
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
    "per_page=100&fields=id%2Cdisplay_name%2Cplace_type"
  )
  data <- req_inat(url)

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
  data <- req_inat(url)

  data$total_results
}


# helper to send a request to iNaturalist
req_inat <- function(url) {
  resp <- httr2::request(url) %>%
      httr2::req_retry(max_tries = 2) %>% 
      httr2::req_throttle(capacity = 50) %>% 
      httr2::req_perform()

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}
