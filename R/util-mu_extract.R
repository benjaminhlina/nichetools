#' Mu extract
#'
#' Extract Bayesian estimates of mue from the function `niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @param data a list object created by `niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html)
#'
#' @examples
#' df_mu <- mu_extract(
#' data = niw_fish_post
#' )
#'
#' @export
mu_extract <- function(data) {

  # Check if data is a list
  if (!inherits(data, "list")) {
    cli::cli_abort("Input 'data' must be a list.")
  }
  # extract mu
  df_mu <- purrr::map(data, purrr::pluck, 1) %>%
    purrr::imap(~ tibble::as_tibble(.x) %>%
           dplyr::mutate(
             metric = "mu",
             sample_name = .y
           )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(sample_name) %>%
    dplyr::mutate(
      sample_number = 1:1000
    ) %>%
    dplyr::ungroup()

  return(df_mu)
}
