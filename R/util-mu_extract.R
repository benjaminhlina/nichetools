#' Mu extract
#'
#' Extract Bayesian estimates of mue from the function `niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @param data a list object created by `niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#'@return Returns a tibble of extracted estimates of mu that are created by
#' niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' The tibble will contain five columns in the following order, metric,
#' sample_name, sample_number, and the names of the isotope
#' columns (e.g., d15n and d13c).
#'
#' @examples
#'
#'
#' df_mu <- mu_extract(
#' data = niw_fish_post
#' )
#'
#' # ---- To understand how niw_fish_post is being created ----
#' # ---- Uncomment the code below to create object from nicheROVER ----
#' # {
#' # library(dplyr)
#' # library(janitor)
#' # library(nicheROVER)
#' # library(nichetools)
#' # library(purrr)
#' # }
#'
#' # gab fish dataframe, and remove sulfer for the example
#' # df <- fish |>
#' #   janitor::clean_names()
#'
#' # create number of samples used in nicheROVER
#' # nsample <- 1000
#'
#' # split the data frame by species and select isotopes of interest
#' # df_split <- df |>
#' # split(.$species) |>
#' # map(~ select(., d15n, d13c))
#'
#' # extract the names of each list to name each object in list
#' # df_names <- df |>
#' # group_by(species) |>
#' # group_keys() |>
#' # ungroup() |>
#' # mutate(
#' # id = 1:nrow(.)
#' # )
#'
#' # name each object in list
#' # names(df_split) <- df_names$species
#'
#' # create niw posterior samples
#' # niw_fish_post <- df_split |>
#' # map(~niw.post(nsample = nsample, X = .))
#'
#'
#' # we can then use mu_extract() to extract mu from niw_fish_post
#' # df_mu <- mu_extract(
#' # data = niw_fish_post
#' # )
#'
#' @export
mu_extract <- function(data) {

  # Check if data is a list
  if (!inherits(data, "list")) {
    cli::cli_abort("Input 'data' must be a list.")
  }

  col_names <- purrr::map(data, purrr::pluck, 1) |>
    purrr::imap(~ tibble::as_tibble(.x)) |>
    dplyr::bind_rows() |>
    names()

  # extract mu
  df_mu <- purrr::map(data, purrr::pluck, 1) |>
    purrr::imap(~ tibble::as_tibble(.x) |>
           dplyr::mutate(
             metric = "mu",
             sample_name = .y
           )
    ) |>
    dplyr::bind_rows() |>
    dplyr::group_by(sample_name) |>
    dplyr::mutate(
      sample_number = 1:1000
    ) |>
    dplyr::ungroup()


  df_mu <- df_mu |>
    dplyr::select(metric:sample_number, all_of(col_names))

  return(df_mu)
}
