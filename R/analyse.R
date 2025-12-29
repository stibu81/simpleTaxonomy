#' Count the Number of Taxa per Rank
#'
#' Count the number of taxa per rank in a `taxonomy_graph` and return
#' the result as a tibble.
#'
#' @param graph a `taxonomy_graph` object
#' @param subgraph the name of a taxon which is to be used as the root of a
#'  subgraph, to which counting is restricted.
#' @param by_rank the name of a rank. Ranks will be counted for each of the
#'  taxa with this rank.
#' @param only_major_ranks should counts only be shown for the major ranks
#'  (Lebewesen, Domäne, Reich, Stamm, Klasse, Ordnung, Familie, Gattung, Art)?
#'
#' @details
#' If `by_rank` is not used, the number of taxa is counted in the complete
#' graph or in the subgraph specified by `subgraph`.
#'
#' `by_rank` can be used to count ranks grouped by the taxa of a given rank.
#' In this case, each row will correspond to a taxon of the requested rank
#' and each column will correspond to a rank.
#'
#' @return
#' a tibble
#'
#' @examples
#' file <- get_example_taxonomy_file()
#' taxonomy <- read_taxonomy(file)
#'
#' # count the complete graph
#' count_ranks(taxonomy)
#'
#' # count only the cats
#' count_ranks(taxonomy, subgraph = "Katzen")
#'
#' # count caniformia by family
#' count_ranks(taxonomy, "Hundeartige", by_rank = "Familie")
#'
#' @export

count_ranks <- function(graph, subgraph = NULL, by_rank = NULL,
                        only_major_ranks = FALSE) {

  # this only works for taxonomy_graph objects
  if (!inherits(graph, "taxonomy_graph")) {
    cli::cli_abort(
      "{deparse(substitute(graph))} is not a taxonomy_graph object."
    )
  }

  if (!is.null(subgraph)) graph <- get_subgraph(graph, subgraph)

  # get the order of the ranks
  ranks_ordered <- rev(available_ranks()$de)

  # to variants:
  # * if by_rank is used, count for each taxon of the selected rank and put
  #   the ranks in the columns.
  # * if by_rank is not used, count everything and put the ranks in the rows.
  if (!is.null(by_rank)) {
    if (length(by_rank) > 1) {
      cli::cli_abort("by_rank must have length one.")
    }
    if (!by_rank %in% ranks_ordered) {
      cli::cli_abort("\"{by_rank}\" is not a valid rank.")
    }
    # "ohne Rang" is not allowed for by_rank because it makes no sense
    if (by_rank == "ohne Rang") {
      cli::cli_abort("\"ohne Rang\" cannot be used for by_rank.")
    }

    # get a subgraph for each taxon of the requested rank
    tax_with_rank <- graph %>%
      as_tibble() %>%
      dplyr::filter(.data$rank == by_rank) %>%
      dplyr::pull("name")

    if (length(tax_with_rank) == 0) {
      cli::cli_abort(
        paste(
          "\"{by_rank}\" is a valid rank, but it does not appear in the",
          "taxonomy graph."
        )
      )
    }

    # get a subgraph for each taxon with the selected rank, count, & transform
    counts <- tax_with_rank %>%
      lapply(\(tax) get_subgraph(graph, tax)) %>%
      lapply(
        \(subgraph) do_count_ranks(subgraph, ranks_ordered, only_major_ranks)
      ) %>%
      stats::setNames(tax_with_rank) %>%
      dplyr::bind_rows(.id = by_rank) %>%
      # the rank used for grouping must be removed in order to avoid duplication
      # of column names
      dplyr::filter(rank != by_rank) %>%
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(by_rank),
        names_from = "rank",
        values_from = "n",
        values_fill = 0
      ) %>%
      dplyr::select(dplyr::any_of(rev(ranks_ordered))) %>%
      # make sure the rank to group by comes first. This is automtaically the
      # case for most ranks, but there are some exceptions
      dplyr::relocate(dplyr::all_of(by_rank), .before = 1) %>%
      # replace the taxon names by labels. The columns with taxon names is the
      # only character column in the tibble.
      dplyr::mutate(dplyr::across(is.character, get_taxon_labels))
  } else {
    counts <- do_count_ranks(graph, ranks_ordered, only_major_ranks)
  }

  counts
}


# helper function that does the actual counting
do_count_ranks <- function(graph, ranks_ordered, only_major_ranks) {
  rank_counts <- graph %>%
    as_tibble() %>%
    dplyr::mutate(rank = ordered(rank, levels = ranks_ordered)) %>%
    dplyr::count(rank) %>%
    dplyr::arrange(dplyr::desc(rank))

  if (only_major_ranks) {
    rank_counts <- rank_counts %>%
      dplyr::filter(.data$rank %in% get_major_ranks())
  }

  rank_counts
}


get_major_ranks <- function() {
  c("Lebewesen", "Dom\u00e4ne", "Reich", "Stamm", "Klasse", "Ordnung",
    "Familie", "Gattung", "Art")
}
