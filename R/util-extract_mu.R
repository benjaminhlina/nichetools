#' extract μ
#'
#' Extract Bayesian estimates of μ from the function `niw.post()` in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @param data a `list` created by the function `niw.post()` in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' @param format a `character` that decided whether the returned object is
#' in long or wide format. Default is `"long"`, with the alternative supplied
#' being `"wide"`.
#'
#' @return Returns a `tibble` of extracted estimates of μ that are created by
#' the function `niw.post()` in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' The `tibble` will contain five columns in the following order, `metric`,
#' `sample_name`, `sample_number`, and the names of the isotope
#' columns supplied to `niw.post()` (e.g., `d15n` and `d13c`).
#'
#' @seealso [nicheROVER::niw.post()]
#' @examples
#' extract_mu(
#' data = niw_fish_post
#' )
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import tidyr
#' @export


extract_mu <- function(data,
                       format = "long") {

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

  # reorder columns
  df_mu <- df_mu |>
    dplyr::select(metric:sample_number, all_of(col_names))


  if (format %in% "long") {
    df_mu <- df_mu |>
      tidyr::pivot_longer(cols = -c(metric, sample_name, sample_number),
                          names_to = "isotope",
                          values_to = "mu_est"
      )

    return(df_mu)
  }

  if (format %in% "wide") {
    return(df_mu)
  }
}
