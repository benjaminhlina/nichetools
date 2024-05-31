#' extract \eqn{\mu}
#'
#' Extract Bayesian estimates of \eqn{\mu} from data objects created by
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html) or
#' [{SIBER}](https://cran.r-project.org/web/packages/SIBER/index.html)..
#'
#' @param data a `list` created by the function `niw.post()` or `siberMVN()`
#' in the package
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html)
#' or [{SIBER}](https://cran.r-project.org/web/packages/SIBER/index.html), respectfully.
#' @param pkg a `character` string that is the name of the package that
#' you're using. Defaults to `"nicheROVER"`.
#' Alternatively the user can supply the argument with `"SIBER"`.
#' @param isotope_a a `character` string to change the column name
#' of the first isotope used in the analysis. Defaults to `"d13c"`.
#' @param isotope_b a `character` string to change the name of second isotope
#' used in the analysis. Defaults to `"d15n"`.
#' @param data_format a `character` string that decided whether the returned object is
#' in long or wide format. Default is `"long"`, with the alternative supplied
#' being `"wide"`.
#'
#' @return Returns a `tibble` of extracted estimates of \eqn{\mu} created by the
#' function `niw.post()` or `siberMVN()` in the packages
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#' and [{SIBER}](https://cran.r-project.org/web/packages/SIBER/index.html).
#'
#' The `tibble` will contain five columns in the following order, `metric`,
#' `sample_name`, `sample_number`, and the names of the isotope
#' columns supplied to `niw.post()` (e.g., `d15n` and `d13c`).
#'
#' @seealso [nicheROVER::niw.post()] and [SIBER::siberMVN()]
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
