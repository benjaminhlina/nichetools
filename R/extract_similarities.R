#' extract similarities
#'
#' Extract niche similarities from objects created by `{SIBER}`.
#'
#' @param data a `list` of results from either `maxLikOverlap()` or
#' `bayesianOverlap()`.
#' @param type description
#' @param community_df a four column data frame. One of the columns has to be named
#' `community` and the data in the column will be `numeric` as a `character`
#' string(e.g., `"1", "2", "3"`). This is the order of the community names
#' and will be used to join the actual community names to the correct data.
#' These are the same class and values required by the function, `createSiberObject()`
#' from [{SIBER}](https://CRAN.R-project.org/package=SIBER).
#' The second column will be the names of the groups that are needed to supply
#' required by the function, `createSiberObject()`
#' from [{SIBER}](https://CRAN.R-project.org/package=SIBER).
#' The third and fourth columns contains the actual names of the communities
#' and groups the user is working with (e.g., `"region"`, `"common_name"`).
#'
#' @examples
#' library(purrr)
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
#' cg_names_within_c <- create_comparisons(cg_names)
#'
#' ml_within_overlap <- cg_names_within_c %>%
#' map(~ maxLikOverlap(.x$cg_1, .x$cg_2, siber_example,
#' p.interval = NULL, n = 100), .progress = TRUE)
#'
#' ml_95_within_com <- extract_similarities(ml_within_overlap, type = "ml",
#' community_df = cg_names)
#'
#' @export
extract_similarities <- function(data,
                                 type = c("bay", "ml"),
                                 community_df = NULL) {



  if (!(type %in% c("bay", "ml"))) {
    cli::cli_abort("Invalid characters for 'type'. Allowed character
    strings are 'bay' or 'ml'.")
  }

  # Check if `community_df` is a four-column data.frame
  if (!is.null(community_df)) {
    if (!is.data.frame(community_df) || ncol(community_df) != 4) {
      cli::cli_abort(c(
        "The `community_df` argument must be a data.frame with exactly four columns.",
        "i" = "Please provide a data.frame with four columns."
      ))
    }
  }

  # column names are community and group of community_df
  if (!any(c("community", "group") %in% colnames(community_df))) {
    cli::cli_abort("The data frame does not contain a column named
                   'community' and 'group'.")
  }

  niche_similarities <- data |>
    dplyr::bind_rows(.id = "compare") |>
    tidyr::separate_wider_delim(compare, delim = "_", names = c("compare_1",
                                                                "compare_2")) |>
    tidyr::separate_wider_delim(compare_1, delim = ".", names = c("community",
                                                                  "group")) |>
    dplyr::left_join(community_df, by = c("community", "group")) |>
    dplyr::rename(
      community_id_1 = community,
      group_id_1 = group,
      community_1 = community_name,
      group_1 = group_name,
    ) |>
    tidyr::separate_wider_delim(compare_2, delim = ".", names = c("community",
                                                                  "group")) |>
    dplyr::left_join(cg_names, by = c("community", "group"))

  if (type %in% "ml") {
    niche_similarities <- niche_similarities |>
      dplyr::rename(
        community_id_2 = community,
        group_id_2 = group,
        community_2 = community_name,
        group_2 = group_name,
        area_1 = area.1,
        area_2 = area.2,
      ) |>
      dplyr::mutate(
        prop_overlap = overlap / ((area_2 + area_1) - overlap)
      )
  }
  if (type %in% "bay") {
    niche_similarities <- niche_similarities |>
      dplyr::rename(
        community_id_2 = community,
        group_id_2 = group,
        community_2 = community_name,
        group_2 = group_name,
        area_1 = area1,
        area_2 = area2,
      ) |>
      dplyr::mutate(
        prop_overlap = overlap / ((area_2 + area_1) - overlap)
      )
  }

  return(niche_similarities)
}
