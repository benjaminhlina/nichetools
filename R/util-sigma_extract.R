#' Sigma extract
#'
#' Extract Bayesian estimates of sigma from the function `niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html).
#'
#' @param data a list object created by `niw.post()` from
#' [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html)
#' @param isotope_a character string to supply for the first
#' isotope used in `niw.post()`. Defaults to `"d15n"`.
#' @param isotope_b character string to supply for the first
#' isotope used in `niw.post()`. Defaults to `"d13c"`.
#'
#' @examples
#'df_sigma_test <- sigma_extract(
#'data = niw_fish_post
#')
#'
#' @export

sigma_extract <-  function(data,
                           isotope_a = NULL,
                           isotope_b = NULL) {
  # Check if data is a list
  if (!is.list(data)) {
    cli::cli_abort("Input 'data' must be a list.")
  }

  # defaults of isotpoep a and b
  if (is.null(isotope_a)) {
    isotope_a <- "d15n"
  }

  if (is.null(isotope_b)) {
    isotope_b <- "d13c"
  }
  # Check if isotope_a is character
  if (!is.character(isotope_a)) {
    cli::cli_abort("The supplied argument for 'isotope_a' must be a character.")
  }

  # Check if isotope_b is character
  if (!is.character(isotope_b)) {
    cli::cli_abort("The supplied argument for 'isotope_b' must be a character.")
  }

  # extract sigm
  df_sigma <- purrr::map(data, pluck, 2) |>
    purrr::imap(~ tibble::as_tibble(.x) |>
           dplyr::mutate(
             metric = "sigma",
             id = c(isotope_a, isotope_b),
             sample_name = .y
           )
    ) |>
   dplyr::bind_rows() |>
    tidyr::pivot_longer(cols = -c("id", "sample_name", "metric"),
                 names_to = "isotope",
                 values_to = "post_sample"
    ) |>
    tidyr::separate(isotope, into = c("isotope", "sample_number"), sep = "\\.")

  return(df_sigma)
}
