#' create comparisons
#'
#' Creates a list with all of the comparisons needed to create  Bayesian and maximum-likelihood
#' estimates for proportion of niche similarities.
#'
#' @param data a `data.frame` that is the names of the community and group names
#' @param comparison a `character`that is either `"within"` or `"among"` indicating
#' whether the comparisons are within a community and between groups or
#' among communities for the same groups.
#'
#' @examples
#'
#' # ---- load siber ----
#' library(SIBER)
#'
#'# ---- create community names data frame ----
#' # uncomment to use
#' # str(demo.siber.data.2)
#'
#' demo.siber.data.2$group_name <- as.factor(demo.siber.data.2$group)
#'
#' demo.siber.data.2$group <- as.numeric(demo.siber.data.2$group_name) |>
#' as.character()
#'
#' demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)
#'
#' demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
#' as.character()
#'
#' cg_names <- demo.siber.data.2 |>
#' dplyr::distinct(community, group, community_names, group_name)
#'
#' # ---- create comparsions ----
#' create_comparisons(cg_names,
#'                   comparison = "within")
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @export


create_comparisons <- function(data,
                               comparison = c("within", "among")) {

  # Check if data is a tibble, data.frame, or data.table
  if (!inherits(data, c("tbl_df", "data.frame", "data.table"))) {
    cli::cli_abort("'data' must be a tibble, data.frame, or data.table.")
  }

   if (is.null(comparison)) {
    comparison <- "within"
  }

  if (length(comparison) != 1 || !(comparison %in% c("within", "among"))) {
    cli::cli_abort("'comparison' must be either 'within' or 'among'.")
  }

  compare_community <- data |>
    dplyr::mutate(
      cg_1 = paste(community, group, sep = "."),
      cg_2 = paste(community, group, sep = "."),
    ) |>
    dplyr::select(cg_1, cg_2) |>
    tidyr::expand(cg_1, cg_2) |>
    dplyr::filter(cg_1 != cg_2) |> # keep when cg_1 and cg_2 are not the same
    tidyr::separate_wider_delim(cg_1, delim = ".", names = c("c_1", "g_1"),
                                cols_remove = FALSE) |>
    tidyr::separate_wider_delim(cg_2, delim = ".", names = c("c_2", "g_2"),
                                cols_remove = FALSE)

  if (comparison %in% "within") {
    compare_community_1 <- compare_community |>
      dplyr::filter(!(c_1 != c_2 ))
  }
  if (comparison %in% "among") {
    compare_community_1 <- compare_community |>
    dplyr::filter(!(g_1 != g_2 ))
  }

  # this is the differences   filter(!(g_1 != g_2 ))


  compare_community_2 <- compare_community_1 |>
    dplyr::select(-c("c_1", "c_2", "g_1", "g_2")) |>
    dplyr::mutate(
      cg_1 = factor(cg_1),
      cg_2 = factor(cg_2)
    ) |>
    dplyr::mutate(
      names = paste(cg_1, cg_2, sep = "_") |>
        factor()
    ) |>
    dplyr::arrange(names) |>
    dplyr::mutate(
      names = as.character(names)
    )

  # create compare filter
  compare_filter <- compare_community_2 |>
    dplyr::distinct(names) |>
    (`[[`)("names") |>
    strsplit("_") |>
    lapply(sort) |>
    sapply(paste, collapse = "_") |>
    unique()

  # filter out duplicates

  compare_community_3 <- compare_community_2 |>
    dplyr::filter(names %in% compare_filter)



  # split
  compare_community_4 <- compare_community_3 |>
    split(~ names) |>
    purrr::map(~ .x |>
                 dplyr::select(-names) |>
                 dplyr::mutate(
                   cg_1 = as.character(cg_1),
                   cg_2 = as.character(cg_2),
                 ))

  return(compare_community_4)
}
