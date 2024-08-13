#' extract Layman metrics
#'
#' Extract Bayesian estimates for the following six layman metrics,
#' \eqn{\delta^{13}}C range,  \eqn{\delta^{15}}N range, total area (TA),
#' distance to centroid (CD), distance to the nearest neighbour (NND), and
#' the standard deviation of the distance to the nearest neighbour (SDNND)
#' from data objects created by
#' [{SIBER}](https://CRAN.R-project.org/package=SIBER). To learn
#' more about the following metrics please review
#' [Layman et al. (2008)](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/0012-9658%282007%2988%5B42%3ACSIRPF%5D2.0.CO%3B2).
#'
#' @param data a `list` created by the function `bayesianLayman()` from the package
#' [{SIBER}](https://CRAN.R-project.org/package=SIBER).
#' @param community_df a two column data frame. One of the columns has to be named
#' `community` and the data in the column will be `numerics` as a `character`
#' string(e.g., `"1", "2", "3"`). This is the order of the community names
#' and will be used to join the actual community names to the correct data.
#' These are the same class and values required by the function, `createSiberObject()`
#' from [{SIBER}](https://CRAN.R-project.org/package=SIBER).
#' The second column contains the actual names of the communities
#' that the user is working with (e.g., `"region"`).
#' @param data_format a `character` string that decides whether the returned object is
#' in long or wide format. Default is `"long"`, with the alternative supplied being `"wide"`.
#' @param isotope_x a `numeric` that will be used in the labeling processes for
#' the range of the x. Default is `13` (e.g., \eqn{\delta}^13 C).
#' This will create a column called `labels`, that will only be created when `data_format`
#' is set to `long`.
#' @param isotope_y a `numeric` that will be used in the labeling processes for
#' the range of the y isotope. Default is `15` (e.g., \eqn{\delta}^15 N).
#' #' This will create a column called `labels`, that will only be created when `data_format`
#' is set to `long`.
#' @param element_x a `character` that will be used in the labeling process for the range of the x
#' isotope. Default is `C` (e.g., \eqn{\delta}^13 C).
#' This will create a column called `labels`, that will only be created when `data_format`
#' is set to `long`.
#' @param element_y  a `character` that will be used in the labeling process for the range of the y
#' isotope. Default is `N` (e.g., \eqn{\delta}^13 N).
#' #' This will create a column called `labels`, that will only be created when `data_format`
#' is set to `long`.
#'
#' @return A `tibble` containing four rows when `data_format` is set to its
#' default which is `long`. These four rows are the following, `community`,
#' `the_name_of_the_communities`, `metric` and `post_est`.
#'
#' @seealso [SIBER::bayesianLayman()] and [SIBER::createSiberObject()]
#' @examples
#'
#' library(nichetools)
#' library(SIBER)
#'
#' # ---- bring in SIBER demo data ----
#' # uncomenet to use
#' # str(demo.siber.data)
#'
#' # ---- create coummnity names data frame ----
#' # uncomment to use
#' # str(demo.siber.data.2)
#'
#' demo.siber.data.2$community_names <- as.factor(demo.siber.data.2$community)
#'
#' demo.siber.data.2$community <- as.numeric(demo.siber.data.2$community_names) |>
#' as.character()
#' c_names <- demo.siber.data.2 |>
#' dplyr::distinct(community, community_names)
#'
#' # ---- create the siber object ----
#' siber.example <- createSiberObject(demo.siber.data)
#'
#' # ---- view Baysain estimates of mu and sigma produced by SIBER ---
#' # uncomment to use
#' # str(post_sam_siber)
#'
#' # ---- extract posterior estimates of mu -----
#'
#' mu_post <- extractPosteriorMeans(siber.example, post_sam_siber)
#'
#' # ---- Bayesian estimates of layman metrics using SIBER ----
#'
#' layman_b <- bayesianLayman(mu.post = mu_post)
#'
#' # ---- use nichetools to extract Baysian estimats of Layman metrics ----
#'
#' layman_be <- extract_layman(layman_b, community_df = c_names)
#'
#' layman_be
#'
#' @import dplyr
#' @import purrr
#' @import SIBER
#' @import tibble
#' @import tidyr
#' @export


extract_layman <- function(data,
                           community_df = NULL,
                           data_format = NULL,
                           isotope_x = NULL,
                           isotope_y = NULL,
                           element_x = NULL,
                           element_y = NULL
) {


  if (!is.list(data)) {
    cli::cli_abort(c(
      "The `data` argument must be a list.",
      "i" = "Please provide data in list format."
    ))
  }

  # Check if `community_df` is a two-column data.frame
  if (!is.null(community_df)) {
    if (!is.data.frame(community_df) || ncol(community_df) != 2) {
      cli::cli_abort(c(
        "The `community_df` argument must be a data.frame with exactly two columns.",
        "i" = "Please provide a data.frame with two columns."
      ))
    }
  }

  if (!"community" %in% colnames(community_df)) {
    cli::cli_abort("The data frame does not contain a column named 'community'.")
  }




  # sett data formatt
  if (is.null(data_format)) {
    data_format <- "long"
  }

  if (!(data_format %in% c("wide", "long"))) {
    cli::cli_abort("Invalid characters for 'data_format'. Allowed character
    strings are 'wide' or 'long'.")
  }

  # ---- set isotopes values -----=
  if (is.null(isotope_x)) {
    isotope_x <- 13
  }

  if (is.null(isotope_y)) {
    isotope_y <- 15
  }

  # ---- set elemental number -----
  if (is.null(element_x)) {
    element_x <- "C"
  }

  if (is.null(element_y)) {
    element_y <- "N"
  }



  df_layman <- data |>
    purrr::map(~ as_tibble(.x)) |>
    dplyr::bind_rows(.id = "community") |>
    dplyr::left_join(community_df, by = "community")

  if (data_format %in% "long") {

    second_column <- names(df_layman)[8]

    df_layman <- df_layman |>
      tidyr::pivot_longer(
      cols = -c({{second_column}}, community),
      names_to = "metric",
      values_to = "post_est"
    )

  return(df_layman)
  }
  if (data_format %in% "wide"){
    return(df_layman)
  }
}
